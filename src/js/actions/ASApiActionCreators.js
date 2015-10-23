import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import Converter from '../AS/Converter';
import Store from '../stores/ASEvaluationStore';

import isNode from 'detect-node';
let [ws] = isNode ?
  [require('ws')] :
  [WebSocket];

var ActionTypes = Constants.ActionTypes;
var wss = new ws(Constants.HOST_WS);

let currentCbs = undefined;
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
  console.log("Client received data from server: " + JSON.stringify(event.data));

  if (event.data === 'ACK') return;

  let msg = JSON.parse(event.data);
  if (msg.result.tag === "Failure") {
    Dispatcher.dispatch({
        type: ActionTypes.GOT_FAILURE,
        errorMsg: msg
      });

    if (isRunningTest) {
      // sometimes we want to test whether it errors, so it fulfills anyways!
      console.log('Fulfilling due to server failure');
      currentCbs.fulfill(msg);
      isRunningTest = false;
    }
  } else {
    if (isRunningTest) {
      currentCbs.fulfill(msg);
      isRunningTest = false;
    }

    switch (msg.action) {
      // TODO: add cases for new
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
      case "Evaluate":
        Dispatcher.dispatch({
          type: ActionTypes.GOT_UPDATED_CELLS,
          updatedCells: msg.payload.contents
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
      case "Clear":
        Dispatcher.dispatch({
          type: ActionTypes.CLEARED,
        });
        break;
      case "Delete":
        if (msg.payload.tag === "PayloadR"){
          Dispatcher.dispatch({
            type: ActionTypes.DELETED_LOCS,
            locs: msg.payload.contents
          });
        } else if (msg.payload.tag === "PayloadWorkbookSheets") {
          Dispatcher.dispatch({
            type: ActionTypes.DELETED_WORKBOOKS,
            workbooks: msg.payload.contents
          });
        }
        break;
      case "EvaluateRepl":
        Dispatcher.dispatch({
          type: ActionTypes.GOT_REPL_RESP,
          response:msg.payload.contents
        });
        break;
    }
  }
};

wss.onopen = (evt) => {
  console.log('WebSockets open');
};

export default {

  /**************************************************************************************************************************/
  /* Sending acknowledge message to server */

  waitForSocketConnection(socket, callback, waitTime) {
    if (typeof(waitTime) == "undefined") {
      waitTime = 0;
    }

    if (waitTime >= 5000 && !refreshDialogShown) {
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
    console.log(`Queueing ${msg.action} message`);
    this.waitForSocketConnection(wss, () => {
      console.log(`Sending ${msg.action} message`);
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
    let msg = Converter.makeClientMessage(Constants.ServerActions.Acknowledge,
                                          "PayloadInit",
                                          {"connUserId": Store.getUserId()});
    console.log("Sending init message: " + JSON.stringify(msg));
    this.send(msg);
  },

  /**************************************************************************************************************************/
  /* Sending admin-related requests to the server */

  getWorkbooks() {
    let msg = Converter.makeClientMessage('Get', 'PayloadList', 'WorkbookSheets');
    this.send(msg);
  },

  close() {
    console.log('Sending close message');
    wss.close();
  },

  // ************************************************************************************************************************
  /* Sending an eval request to the server */

  /* This function is called by handleEvalRequest in the eval pane */
  evaluate(asIndex,xpObj){
    let asCell = Converter.makeEvalCell(asIndex, xpObj),
        msg = Converter.makeClientMessage(Constants.ServerActions.Evaluate,
                                          "PayloadCL",
                                          [asCell]);
    this.send(msg);
  },

  evaluateRepl(xpObj){
    let msg = Converter.makeClientMessage(Constants.ServerActions.Repl, "PayloadXp", {
      tag: "Expression",
      expression: xpObj.exp,
      language: xpObj.lang
    });
    this.send(msg);
  },
  /**************************************************************************************************************************/
  /* Sending undo/redo/clear messages to the server */

  undo(){
    let msg = Converter.makeClientMessage(Constants.ServerActions.Undo, "PayloadN", []);;
    this.send(msg);
  },
  redo(){
    let msg = Converter.makeClientMessage(Constants.ServerActions.Redo, "PayloadN", []);
    this.send(msg);
  },
  clear(){
    let msg = Converter.makeClientMessage(Constants.ServerActions.Clear, "PayloadN", []);
    this.send(msg);
  },
  deleteIndices(locs) {
    let msg = Converter.makeClientMessage(Constants.ServerActions.Delete, "PayloadLL", locs);
    this.send(msg);
  },
  deleteRange(rng) {
    let msg = Converter.makeClientMessage(Constants.ServerActions.Delete, "PayloadR", rng);
    this.send(msg);
  },


  /**************************************************************************************************************************/
  /* Sending get messages to the server */
  addTags(tags, loc) {
    let msg = Converter.makeClientMessageRaw(Constants.ServerActions.AddTags, {
      "tag": "PayloadTags",
      "tags": tags,
      "tagsLoc": loc
    });
    this.send(msg);
  },
  removeTags(tags, loc) {
    let msg = Converter.makeClientMessageRaw(Constants.ServerActions.RemoveTags, {
      "tag": "PayloadTags",
      "tags": tags,
      "tagsLoc": loc
    });
    this.send(msg);
  },
  copy(fromRng, toRng) {
    let msg = Converter.makeClientMessageRaw(Constants.ServerActions.Copy, {
      tag: "PayloadPaste",
      copyRange: fromRng,
      copyTo: toRng
    });
    this.send(msg);
  },
  cut(fromRng, toRng) {
    let msg = Converter.makeClientMessageRaw(Constants.ServerActions.Cut, {
      tag: "PayloadPaste",
      copyRange: fromRng,
      copyTo: toRng
    });
    this.send(msg);
  },
  simplePaste(cells){
    let msg = Converter.makeClientMessage(Constants.ServerActions.Evaluate, "PayloadCL",cells);
    this.send(msg);
  },

  getIndices(locs) {
    let msg = Converter.makeClientMessage(Constants.ServerActions.Get, "PayloadLL", locs);
    this.send(msg);
  },

  getRange(rng) {
    let msg = Converter.makeClientMessage(Constants.ServerActions.Get, "PayloadR", rng);
    this.send(msg);
  },

  openSheet(sheet) {
    let msg = Converter.makeClientMessage(Constants.ServerActions.Open, "PayloadS", sheet);
    this.send(msg);
  },

  createSheet() {
    let wbs = Converter.makeWorkbookSheet();
    let msg = Converter.makeClientMessage(Constants.ServerActions.New,
      "PayloadWorkbookSheets",
      [wbs]);
    this.send(msg);
  },

  createWorkbook() {
    let wb = Converter.makeWorkbook();
    let msg = Converter.makeClientMessage(Constants.ServerActions.New,
      "PayloadWB",
      wb);
    this.send(msg);
  },

  updateViewingWindow(vWindow) {
    let msg = Converter.makeClientMessage(Constants.ServerActions.UpdateWindow,
                                              "PayloadW",
                                              vWindow);
    this.send(msg);
  },


  test(f, cbs) {
    currentCbs = cbs;
    isRunningTest = true;

    f();
  },

  testSync(f, cbs) {
    currentCbs = cbs;
    isRunningSyncTest = true;

    f();
  }
};
