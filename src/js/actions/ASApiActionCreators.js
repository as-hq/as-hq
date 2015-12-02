/* @flow */

import type {
  ASAction,
  GotFailureAction
} from '../types/Actions';

import type {
  Callback
} from '../types/Base';

import type {
  NakedRange,
  NakedIndex,
  ASIndex,
  ASRange,
  ASLanguage,
  ASExpression,
  ASValue,
  ASSheet,
  ASCellProp,
  ASCell,
  VAlignType,
  HAlignType
} from '../types/Eval';

import type {
  Direction,
  OpenResponse,
  UndoResponse, // TODO: when 582 is fixed, eliminate this and sum type
  RedoResponse,
  GetResponse,
  UpdateWindowResponse,
  JumpSelectResponse,
  EvaluateReplResponse,
  EvaluateHeaderResponse,
  ASBackendResult,
  ASBackendPayload,
  ASServerMessage,
  ASClientMessage,
  ASAPICallbackPair,
  CondFormatRule
} from '../types/Messages';

import type {
  ASSelection,
  ASClientWindow,
  ASClientExpression
} from '../types/State';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import T from '../AS/Types';
import TC from '../AS/TypeConversions';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';
import ws from '../AS/PersistentWebSocket';

let ActionTypes = Constants.ActionTypes;
let wss: ws = new ws(Util.getHostUrl());

let currentCbs: ?ASAPICallbackPair = undefined;
let uiTestMode: boolean = false;
let isRunningTest: boolean = false;
let isRunningSyncTest: boolean = false;
let refreshDialogShown: boolean = false;

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

wss.onmessage = (event: MessageEvent) => {
  if (event.data === 'ACK') return;

  logDebug("Client received data from server: " + event.data.toString());

  if (event.data instanceof Blob) {
    console.log("Received binary data from server.");
    let fName = Store.getCurrentSheet().sheetId + ".as";
    // #anand event.data typecasts to Blob, because we already checked the instance above
    // and flow doesn't understand that event.data is type DOMString | Blob | ...
    let f = Util.blobToFile(((event.data: any): Blob), fName);
    Util.promptSave(f);
  } else {
    let msg: ASServerMessage = JSON.parse(event.data);
    if (msg.result.tag === "Failure") {
      Dispatcher.dispatch({
        _type: 'GOT_FAILURE',
        action: msg.action,
        errorMsg: msg.result.failDesc
      });

      if (isRunningTest && currentCbs) {
        // sometimes we want to test whether it errors, so it fulfills anyways!
        logDebug('Fulfilling due to server failure');
        currentCbs.fulfill(msg);
        isRunningTest = false;
      }
    } else {
      if (isRunningTest && (!uiTestMode || (msg.action != 'UpdateWindow' && msg.action != 'Open')) && currentCbs) {
        currentCbs.fulfill(msg);
        isRunningTest = false;
      }

      switch (msg.action) {
      case "New":
        if (msg.payload.tag === "PayloadWorkbookSheets") {
          Dispatcher.dispatch({
            _type: 'GOT_NEW_WORKBOOKS',
            workbooks: msg.payload.contents
          });
        }
        break;
      case "NoAction":
        break;
      case "Acknowledge":
        break;
      case "Open":
        Dispatcher.dispatch({
          _type: 'GOT_OPEN',
          expressions: msg.payload.initHeaderExpressions
        });
        Dispatcher.dispatch({
          _type: 'GOT_UPDATED_RULES',
          rules: msg.payload.initCondFormatRules
        });
        break;
      case "Undo":
        Dispatcher.dispatch({
          _type: 'GOT_UNDO',
          commit: msg.payload.contents
        });
        break;
      case "Redo":
        Dispatcher.dispatch({
          _type: 'GOT_REDO',
          commit: msg.payload.contents
        });
       break;
      case "Update":
        if (msg.result.tag === "DecoupleDuringEval") {
          Dispatcher.dispatch({
            _type: 'EVAL_TRIED_TO_DECOUPLE',
          });
        } else if (msg.payload.tag === "PayloadCL") {
          Dispatcher.dispatch({
            _type: 'GOT_UPDATED_CELLS',
            updatedCells: msg.payload.contents
          });
        } else if (msg.payload.tag === "PayloadWorkbookSheets") {
          Dispatcher.dispatch({
            _type: 'GOT_UPDATED_WORKBOOKS',
            workbooks: msg.payload.contents
          });
        }
        break;
      case "Get":
        Dispatcher.dispatch({
          _type: 'FETCHED_CELLS',
          newCells: msg.payload.contents
        });
        break;
      case 'SetCondFormatRules':
        Dispatcher.dispatch({
          _type: 'FETCHED_CELLS',
          newCells: msg.payload.condFormatCellsUpdated
        });
        Dispatcher.dispatch({
          _type: 'GOT_UPDATED_RULES',
          rules: msg.payload.condFormatRulesResult
        });
        break;
      //Functionally equivalent to "Get", but useful to be able to distinguish for tests
      case "UpdateWindow":
        Dispatcher.dispatch({
          _type: 'FETCHED_CELLS',
          newCells: msg.payload.contents
        });
        break;
      case "Clear":
        if (msg.payload.tag === "PayloadS") {
          Dispatcher.dispatch({
            _type: 'CLEARED_SHEET',
            sheetId: msg.payload.contents.sheetId
          });
        } else {
          Dispatcher.dispatch({
            _type: 'CLEARED'
          });
        }
        break;
      case "JumpSelect":
        Dispatcher.dispatch({
          _type: 'GOT_SELECTION',
          newSelection: msg.payload
        });
        break;
      case "Delete":
        if (msg.result.tag === "DecoupleDuringEval") {
          Dispatcher.dispatch({
            _type: 'EVAL_TRIED_TO_DECOUPLE'
          });
        } else if (msg.payload.tag === "PayloadDelete") {
          Dispatcher.dispatch({
            _type: 'DELETED_LOCS',
            deletedRange: msg.payload.contents[0],
            updatedCells: msg.payload.contents[1]
          });
        } else if (msg.payload.tag === "PayloadWorkbookSheets") {
          Dispatcher.dispatch({
            _type: 'DELETED_WORKBOOKS',
            workbooks: msg.payload.contents
          });
        } // no case for PayloadWB ??
        break;
      case "EvaluateRepl":
        Dispatcher.dispatch({
          _type: 'GOT_REPL_RESPONSE',
          response: msg.payload.contents
        });
        break;
      case "EvaluateHeader":
        Dispatcher.dispatch({
          _type: 'GOT_EVAL_HEADER_RESPONSE',
          response: msg.payload.contents
        });
        break;
      case "Find":
        // TODO
      /*
        let toClientLoc = function(x) {
          return {row:x.index[1],col:x.index[0]};
        };
        let clientLocs = msg.payload.contents.map(toClientLoc);
        logDebug("GOT BACK FIND RESPONSE: " + JSON.stringify(clientLocs));
        Dispatcher.dispatch({
          _type: 'GOT_FIND',
          findLocs:clientLocs
        }); */
        break;
      case "Import":
        Dispatcher.dispatch({
          _type: 'GOT_IMPORT',
          newCells: msg.payload.contents
        });
        break;
      }
    }
  }
};

wss.onopen = (evt) => {
  logDebug('WebSockets open');
};

export default {
  send(msg: ASClientMessage) {
    logDebug(`Queueing ${msg.action} message`);
    wss.waitForConnection((innerClient: WebSocket) => {
      logDebug(`Sending ${msg.action} message`);
      logDebug(JSON.stringify(msg));
      innerClient.send(JSON.stringify(msg));

      /* for testing */
      if (msg.action === 'Acknowledge' && isRunningTest && currentCbs) {
        isRunningTest = false;
        currentCbs.fulfill();
      } else if (isRunningSyncTest && currentCbs) {
        isRunningSyncTest = false;
        currentCbs.fulfill();
      }
    });
  },

  initMessage() {
    let msg: ASClientMessage = TC.makeClientMessage(Constants.ServerActions.Acknowledge,
      "PayloadInit",
      {"connUserId": Store.getUserId(),
        "connSheetId": Store.getCurrentSheet().sheetId});
    // logDebug("Sending init message: " + JSON.stringify(msg));
    this.send(msg);
  },

  ackMessage(innerClient: WebSocket) {
    let msg: ASClientMessage = TC.makeClientMessage(Constants.ServerActions.Acknowledge,
      'PayloadN', []);
    innerClient.send(JSON.stringify(msg));
  },

  reinitialize() {
    this.initMessage();
    this.openSheet();
    this.updateViewingWindow(
      TC.rangeToASWindow(Store.getViewingWindow().range)
    );
  },

  initialize() {
    wss.sendAck = this.ackMessage;
    wss.beforereconnect = () => { this.reinitialize(); };

    this.initMessage();
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

  export(sheet: ASSheet) {
    let msg = TC.makeClientMessage('Export', 'PayloadS', sheet);
    this.send(msg);
  },

  import(file: File) {
    // any typecast necessary because wss.send is an overloaded, untyped function...
    wss.send(((file: any): string), {binary: true});
  },

  // ************************************************************************************************************************
  /* Sending an eval request to the server */

  /* This function is called by handleEvalRequest in the eval pane */
  evaluate(origin: NakedIndex, xpObj: ASClientExpression) {
    let asIndex = TC.simpleToASIndex(origin),
        asCell = TC.makeEvalCell(asIndex, xpObj),
        msg = TC.makeClientMessage(Constants.ServerActions.Evaluate,
                                          "PayloadCL",
                                          [asCell]);
    this.send(msg);
  },

  evaluateHeader(expression: string, language: ASLanguage) {
    let msg = TC.makeClientMessage(Constants.ServerActions.EvalHeader, "PayloadXp", {
      tag: "Expression",
      expression: expression,
      language: language
    });
    this.send(msg);
  },

  evaluateRepl(xpObj: ASExpression) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Repl, "PayloadXp", {
      tag: "Expression",
      expression: xpObj.expression,
      language: xpObj.language
    });
    this.send(msg);
  },

  decouple() {
    let msg: ASClientMessage = TC.makeClientMessage(Constants.ServerActions.Decouple,
      "PayloadN", []);
    this.send(msg);
  },
  /**************************************************************************************************************************/
  /* Sending undo/redo/clear messages to the server */

  undo() {
    let msg = TC.makeClientMessage(Constants.ServerActions.Undo, "PayloadN", []);;
    this.send(msg);
  },
  redo() {
    let msg = TC.makeClientMessage(Constants.ServerActions.Redo, "PayloadN", []);
    this.send(msg);
  },
  clear() {
    let msg = TC.makeClientMessage(Constants.ServerActions.Clear, "PayloadN", []);
    this.send(msg);
  },
  clearSheet() {
    let msg = TC.makeClientMessage(Constants.ServerActions.Clear,
                                   "PayloadS",
                                   Store.getCurrentSheet());
    this.send(msg);
  },
  find(findText: string) {
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
  jumpSelect(
    range: NakedRange,
    origin: NakedIndex,
    isShifted: boolean,
    direction: Direction
  ) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.JumpSelect, {
      tag: "PayloadJump",
      isShifted: isShifted,
      jumpRange: TC.simpleToASRange(range),
      jumpOrigin: TC.simpleToASIndex(origin),
      jumpDirection: "D" + direction
    });
    this.send(msg);
  },
  bugReport(report: string) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.BugReport, {
      tag: "PayloadText",
      text: report,
    });
    this.send(msg);
  },
  deleteIndices(locs: Array<ASIndex>) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Delete, "PayloadLL", locs);
    this.send(msg);
  },
  deleteRange(rng: ASRange) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Delete, "PayloadR", rng);
    this.send(msg);
  },

  toggleProp(prop: ASCellProp, rng: NakedRange) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.ToggleProp, {
      "tag": "PayloadProp",
      "prop": {tag: prop, contents: []},
      "tagRange": TC.simpleToASRange(rng)
    });
    this.send(msg);
  },

  setProp(prop: any, rng: NakedRange) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.SetProp, {
      "tag": "PayloadProp",
      "prop": prop,
      "tagRange": TC.simpleToASRange(rng)
    });
    this.send(msg);
  },

  setTextColor(contents: string, rng: NakedRange) {
    let prop = {
      tag: "TextColor",
      contents: contents
    };
    this.setProp(prop, rng);
  },

  setFillColor(contents: string, rng: NakedRange) {
    let prop = {
      tag: "FillColor",
      contents: contents
    };
    this.setProp(prop, rng);
  },

  setVAlign(contents: VAlignType, rng: NakedRange) {
    let prop = {
      tag: "VAlign",
      contents: contents
    };
    this.setProp(prop, rng);
  },

  setHAlign(contents: HAlignType, rng: NakedRange) {
    let prop = {
      tag: "HAlign",
      contents: contents
    };
    this.setProp(prop, rng);
  },

  setFontSize(contents: number, rng: NakedRange) {
    let prop = {
      tag: "FontSize",
      contents: contents
    };
    this.setProp(prop, rng);
  },

  setFontName(contents: string, rng: NakedRange) {
    let prop = {
      tag: "FontName",
      contents: contents
    };
    this.setProp(prop, rng);
  },

  setFormat(formatType: string, rng: NakedRange) {
    let formatProp = {
      tag: "ValueFormat",
      formatType: formatType
    };
    this.setProp(formatProp, rng);
  },

  setUrl(urlLink: string, rng: NakedRange) {
    let prop = {
      tag: "URL",
      urlLink: urlLink
    };
    this.setProp(prop, rng);
  },

  // Image tags actually have data, so the message is a bit different
  setImageProp(val: {
    imageWidth: number;
    imageHeight: number;
    imageOffsetX: number;
    imageOffsetY: number;
  }, rng: NakedRange) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.SetProp, {
      "tag": "PayloadTag",
      "cellTag": {
        tag: "ImageData",
        imageHeight: val.imageHeight,
        imageWidth: val.imageWidth,
        imageOffsetX: val.imageOffsetX,
        imageOffsetY: val.imageOffsetY
      },
      "tagRange": TC.simpleToASRange(rng)
    });
    this.send(msg);
  },

  drag(activeRng: NakedRange, dragRng: NakedRange) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.Drag, {
      tag: "PayloadDrag",
      initialRange: TC.simpleToASRange(activeRng),
      dragRange: TC.simpleToASRange(dragRng)
    });
    this.send(msg);
  },

  copy(fromRng: ASRange, toRng: ASRange) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.Copy, {
      tag: "PayloadPaste",
      copyRange: fromRng,
      copyTo: toRng
    });
    this.send(msg);
  },

  cut(fromRng: ASRange, toRng: ASRange) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.Cut, {
      tag: "PayloadPaste",
      copyRange: fromRng,
      copyTo: toRng
    });
    this.send(msg);
  },

  pasteSimple(cells: Array<ASCell>) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Evaluate, "PayloadCL", cells);
    this.send(msg);
  },

  getIndices(locs: Array<ASIndex>) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Get, "PayloadLL", locs);
    this.send(msg);
  },

  getRange(rng: ASRange) {
    let msg = TC.makeClientMessage(Constants.ServerActions.Get, "PayloadR", rng);
    this.send(msg);
  },

  repeat(sel: ASSelection) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.Repeat, {
      tag: "PayloadSelection",
      selectionRange: TC.simpleToASRange(sel.range),
      selectionOrigin: TC.simpleToASIndex(sel.origin)
    });
    this.send(msg);
  },

  insertCol(c: number) {
    let mutateType = {
      tag: "InsertCol",
      insertColNum: c
    };
    let msg = TC.makeClientMessage(Constants.ServerActions.MutateSheet, "PayloadMutate", mutateType);
    this.send(msg);
  },

  insertRow(r: number) {
    let mutateType = {
      tag: "InsertRow",
      insertRowNum: r
    };
    let msg = TC.makeClientMessage(Constants.ServerActions.MutateSheet, "PayloadMutate", mutateType);
    this.send(msg);
  },

  deleteCol(c: number) {
    let mutateType = {
      tag: "DeleteCol",
      deleteColNum: c
    };
    let msg = TC.makeClientMessage(Constants.ServerActions.MutateSheet, "PayloadMutate", mutateType);
    this.send(msg);
  },

  deleteRow(r: number) {
    let mutateType = {
      tag: "DeleteRow",
      deleteRowNum: r
    };
    let msg = TC.makeClientMessage(Constants.ServerActions.MutateSheet, "PayloadMutate", mutateType);
    this.send(msg);
  },

  dragCol(c1: number, c2: number) {
    let mutateType = {
      tag: "DragCol",
      oldColNum: c1,
      newColNum: c2
    };
    let msg = TC.makeClientMessage(Constants.ServerActions.MutateSheet, "PayloadMutate", mutateType);
    this.send(msg);
  },

  dragRow(r1: number, r2: number) {
    let mutateType = {
      tag: "DragRow",
      oldRowNum: r1,
      newRowNum: r2
    };
    let msg = TC.makeClientMessage(Constants.ServerActions.MutateSheet, "PayloadMutate", mutateType);
    this.send(msg);
  },

  // @optional mySheet
  openSheet(mySheet?: ASSheet) {
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

  setCondFormattingRules(condFormatRules: Array<CondFormatRule>) {
    let msg = TC.makeClientMessageRaw(Constants.ServerActions.SetCondFormatRules, {
      tag: "PayloadCondFormat",
      condFormatRules: condFormatRules
    });
    this.send(msg);
  },

  updateViewingWindow(vWindow: ASClientWindow) {
    let msg = TC.makeClientMessage(Constants.ServerActions.UpdateWindow,
      "PayloadW",
      vWindow);
    this.send(msg);
  },


  /**************************************************************************************************************************/
  /* Testing */

  withWS<A>(fn: (pws: ws) => A): A {
    return fn(wss);
  },

  test(f: Callback, cbs: ASAPICallbackPair) {
    currentCbs = cbs;
    isRunningTest = true;

    f();
  },

  testSync(f: Callback, cbs: ASAPICallbackPair) {
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
