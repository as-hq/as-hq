import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import T from '../AS/Types';
import TC from '../AS/TypeConversions';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';

import isNode from 'detect-node';
let [ws] = isNode ?
  [require('ws')] :
  [WebSocket];

let ActionTypes = Constants.ActionTypes;
let wss = new ws(Util.getHostUrl());

let currentCbs = undefined;
let uiTestMode = false;
let isRunningTest = false;
let isRunningSyncTest = false;
let refreshDialogShown = false;

/**************************************************************************************************************************/

/*
  This action creator class serves two purposes
  1) Create messages for the server and send them
  2) Take messages received from the server and send them to dispatch
*/

/**************************************************************************************************************************/
/*
  Called whenever the server returns a message
  Depending on the action type of the message, calls dispatcher differently to propagate to stores
  Converts server to client types before going further
*/

wss.onmessage = function (event) {
  logDebug("Client received data from server: " + JSON.stringify(event.data));

  if (event.data === 'ACK') return;

  let msg = JSON.parse(event.data);
  if (msg.result.tag === "Failure") {
    Dispatcher.dispatch({
        type: ActionTypes.GOT_FAILURE,
        errorMsg: msg
      });

    if (isRunningTest) {
      // sometimes we want to test whether it errors, so it fulfills anyways!
      logDebug('Fulfilling due to server failure');
      currentCbs.fulfill(msg);
      isRunningTest = false;
    }
  } else {
    if (isRunningTest && msg.action != 'UpdateWindow') {
      currentCbs.fulfill(msg);
      isRunningTest = false;
    }

    switch (msg.action) {
      case "New":
        if (msg.payload.tag === "PayloadWorkbookSheets") {
          Dispatcher.dispatch({
            type: ActionTypes.GOT_NEW_WORKBOOKS,
            workbooks: msg.payload.contents
          });
        }
        break;
      case "NoAction":
        break;
      case "Acknowledge":
        break;
      case "Undo":
        Dispatcher.dispatch({
          type: ActionTypes.GOT_UNDO,
          commit: msg.payload.contents
        });
        break;
      case "Redo":
        Dispatcher.dispatch({
          type: ActionTypes.GOT_REDO,
          commit: msg.payload.contents
        });
       break;
      case "Update":
        if (msg.payload.tag === "PayloadCL"){
          Dispatcher.dispatch({
            type: ActionTypes.GOT_UPDATED_CELLS,
            updatedCells: msg.payload.contents
          });
        } else if (msg.payload.tag === "PayloadWorkbookSheets") {
          Dispatcher.dispatch({
            type: ActionTypes.GOT_UPDATED_WORKBOOKS,
            workbooks: msg.payload.contents
          });
        }
        break;
      case "Get":
        Dispatcher.dispatch({
          type: ActionTypes.FETCHED_CELLS,
          newCells: msg.payload.contents
        });
        break;
      //Functionally equivalent to "Get", but useful to be able to distinguish for tests
      case "UpdateWindow":
        Dispatcher.dispatch({
          type: ActionTypes.FETCHED_CELLS,
          newCells: msg.payload.contents
        });
        break;
      case "Clear":
        if (msg.payload.tag === "PayloadS") {
          Dispatcher.dispatch({
            type: ActionTypes.CLEARED_SHEET,
            sheetId: msg.payload.sheetId
          });
        } else {
          Dispatcher.dispatch({
            type: ActionTypes.CLEARED
          });
        }
        break;
      case "JumpSelect":
        Dispatcher.dispatch({
          type: ActionTypes.GOT_SELECTION,
          newSelection: msg.payload
        });
        break;
      case "Delete":
        if (msg.payload.tag === "PayloadDelete") {
          Dispatcher.dispatch({
            type: ActionTypes.DELETED_LOCS,
            deletedRange: msg.payload.contents[0], 
            updatedCells: msg.payload.contents[1]
          });
        } else if (msg.payload.tag === "PayloadWorkbookSheets") {
          Dispatcher.dispatch({
            type: ActionTypes.DELETED_WORKBOOKS,
            workbooks: msg.payload.contents
          });
        } // no case for PayloadWB ?? 
        break;
      case "EvaluateRepl":
        Dispatcher.dispatch({
          type: ActionTypes.GOT_REPL_RESP,
          response:msg.payload.contents
        });
        break;
      case "Find":
        let toClientLoc = function(x){
          return {row:x.index[1],col:x.index[0]};
        };
        let clientLocs = msg.payload.contents.map(toClientLoc);
        logDebug("GOT BACK FIND RESPONSE: " + JSON.stringify(clientLocs));
        Dispatcher.dispatch({
          type: ActionTypes.GOT_FIND,
          findLocs:clientLocs
        });
        break;
    }
  }
};

wss.onopen = (evt) => {
  logDebug('WebSockets open');
};

export default {

  /**************************************************************************************************************************/
  /* Sending acknowledge message to server */

  waitForSocketConnection(socket, callback, waitTime) {
    if (typeof(waitTime) == "undefined") {
      waitTime = 0;
    }

    if (waitTime >= 2000 && !refreshDialogShown && !Constants.isDebug) {
      alert("The connection with the server appears to have been lost. Please refresh the page.");
      refreshDialogShown = true;
      waitTime = 0;
    }

    setTimeout(() => {
      if (socket.readyState === 1) {
        if(callback != null){
          callback();
        }
        return;
      } else {
        this.waitForSocketConnection(socket, callback, waitTime + 5);
      }
    }, 5);
  }, // polling socket for readiness: 5 ms

  send(msg) {
    logDebug(`Queueing ${msg.action} message`);
    this.waitForSocketConnection(wss, () => {
      logDebug(`Sending ${msg.action} message`);
      logDebug(JSON.stringify(msg));
      wss.send(JSON.stringify(msg));

      /* for testing */
      if (msg.action === 'Acknowledge' && isRunningTest) {
        isRunningTest = false;
        currentCbs.fulfill();
      } else if (isRunningSyncTest) {
        isRunningSyncTest = false;
        currentCbs.fulfill();
      }
    });
  },

  initialize() {
    let msg = TC.makeClientMessage(Constants.ServerActions.Acknowledge,
                                          "PayloadInit",
                                          {"connUserId": Store.getUserId(),
                                           "connSheetId": Store.getCurrentSheet().sheetId});
    logDebug("Sending init message: " + JSON.stringify(msg));
    this.send(msg);
  },

  /**************************************************************************************************************************/
  /* Sending admin-related requests to the server */

  getWorkbooks() {
    let msg = TC.makeClientMessage('Get', 'PayloadList', 'WorkbookSheets');
    this.send(msg);
  },

  close() {
    logDebug('Sending close message');
    wss.close();
  },

  // ************************************************************************************************************************
  /* Sending an eval request to the server */

  /* This function is called by handleEvalRequest in the eval pane */
  evaluate(asIndex,xpObj){
    let asCell = TC.makeEvalCell(asIndex, xpObj),
        msg = TC.makeClientMessage(Constants.ServerActions.Evaluate,
                                          "PayloadCL",
                                          [asCell]);
    this.send(msg);
  },

  evaluateRepl(xpObj){
    let msg = TC.makeClientMessage(Constants.ServerActions.Repl, "PayloadXp", {
      tag: "Expression",
      expression: xpObj.expression,
      language: xpObj.language
    });
    this.send(msg);
  },
  /**************************************************************************************************************************/
  /* Sending undo/redo/clear messages to the server */

  undo(){
    let msg = TC.makeClientMessage(Constants.ServerActions.Undo, "PayloadN", []);;
    this.send(msg);
  },
  redo(){
    let msg = TC.makeClientMessage(Constants.ServerActions.Redo, "PayloadN", []);
    this.send(msg);
  },
  clear(){
    let msg = TC.makeClientMessage(Constants.ServerActions.Clear, "PayloadN", []);
    this.send(msg);
  },
  clearSheet() {
    let msg = TC.makeClientMessage(Constants.ServerActions.Clear,
                                   "PayloadS",
                                   Store.getCurrentSheet());
    this.send(msg);
  },
  find(findText){
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.Find, {
      tag: "PayloadFind",
      findText: findText,
      matchWithCase:false,
      matchType:0,
      currentSheet: "INIT_SHEET_ID",
      matchFullContents:false
    });
    this.send(msg);
  },
  jumpSelect(range, origin, isShifted, direction) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.JumpSelect, {
      tag: "PayloadJump",
      isShifted: isShifted,
      jumpRange: TC.simpleToASRange(range),
      jumpOrigin: TC.simpleToASIndex(origin),
      jumpDirection: "D" + direction
    });
    this.send(msg);
  },
  bugReport(report) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.BugReport, {
      tag: "PayloadText",
      text: report,
    });
    this.send(msg);
  },
  deleteIndices(locs) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Delete, "PayloadLL", locs);
    this.send(msg);
  },
  deleteRange(rng) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Delete, "PayloadR", rng);
    this.send(msg);
  },


  /**************************************************************************************************************************/
  /* Sending get messages to the server */
  addTags(tags, loc) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.AddTags, {
      "tag": "PayloadTags",
      "tags": tags,
      "tagsLoc": loc
    });
    this.send(msg);
  },
  removeTags(tags, loc) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.RemoveTags, {
      "tag": "PayloadTags",
      "tags": tags,
      "tagsLoc": loc
    });
    this.send(msg);
  },
  copy(fromRng, toRng) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.Copy, {
      tag: "PayloadPaste",
      copyRange: fromRng,
      copyTo: toRng
    });
    this.send(msg);
  },
  cut(fromRng, toRng) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.Cut, {
      tag: "PayloadPaste",
      copyRange: fromRng,
      copyTo: toRng
    });
    this.send(msg);
  },

  pasteSimple(cells){
    let msg = TC.makeClientMessage(Constants.ServerActions.Evaluate, "PayloadCL", cells);
    this.send(msg);
  },

  getIndices(locs) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Get, "PayloadLL", locs);
    this.send(msg);
  },

  getRange(rng) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Get, "PayloadR", rng);
    this.send(msg);
  },

  repeat(sel) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.Repeat, {
      tag: "PayloadSelection",
      selectionRange: TC.simpleToASRange(sel.range),
      selectionOrigin: TC.simpleToASIndex(sel.origin)
    });
    this.send(msg);
  },

// @optional mySheet
  openSheet(mySheet) {
    let sheet = mySheet || Store.getCurrentSheet(),
        msg = TC.makeClientMessage(Constants.ServerActions.Open, "PayloadS", sheet);
    this.send(msg);
  },

  createSheet() {
    let wbs = TC.makeWorkbookSheet();
    let msg = TC.makeClientMessage(Constants.ServerActions.New,
      "PayloadWorkbookSheets",
      [wbs]);
    this.send(msg);
  },

  createWorkbook() {
    let wb = TC.makeWorkbook();
    let msg = TC.makeClientMessage(Constants.ServerActions.New,
      "PayloadWB",
      wb);
    this.send(msg);
  },

  updateViewingWindow(vWindow) {
    let msg = TC.makeClientMessage(Constants.ServerActions.UpdateWindow,
      "PayloadW",
      vWindow);
    this.send(msg);
  },


  /**************************************************************************************************************************/
  /* Testing */

  test(f, cbs) {
    currentCbs = cbs;
    isRunningTest = true;

    f();
  },

  testSync(f, cbs) {
    currentCbs = cbs;
    isRunningSyncTest = true;

    f();
  },

  setUITestMode() {
    uiTestMode = true;
  },

  unsetUITestMode() {
    uiTestMode = false;
  }
};