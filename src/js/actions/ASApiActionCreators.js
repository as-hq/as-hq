/* @flow */

import type {
  ASAction,
  GotFailureAction
} from '../types/Actions';

import type {
  Callback
} from '../types/Base';

import type {
  ASLanguage,
  ASExpression,
  ASValue,
  ASSheet,
  ASCellProp,
  VAlignType,
  HAlignType
} from '../types/Eval';

import type {
  Direction,
  ClientMessage,
  ServerMessage,
  ServerAction,
  ASAPICallbackPair,
  ClearSheetServer,
  UpdateWindow,
  SetProp,
  Delete,
  ToggleProp,
  Evaluate,
  EvalInstruction
} from '../types/Messages';

import type {
  SheetUpdate,
  CondFormatRuleUpdate,
  Update,
  UpdateTemplate
} from '../types/Updates';

import type {
  BarIndex
} from '../types/Bar';

import type {
  ASClientWindow,
  ASClientExpression
} from '../types/State';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

import U from '../AS/Util';

import ASCell from '../classes/ASCell';
import ASCondFormatRule from '../classes/ASCondFormatRule';
import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

import CellStore from '../stores/ASCellStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import ws from '../AS/PersistentWebSocket';

let ActionTypes = Constants.ActionTypes;
console.log("GOT URL: " + Constants.getBackendUrl('ws', Constants.BACKEND_WS_PORT));
let wss: ws = new ws(Constants.getBackendUrl('ws', Constants.BACKEND_WS_PORT));

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
    logDebug("Received binary data from server.");
    let fName = SheetStateStore.getCurrentSheet().sheetId + ".as";
    // #anand event.data typecasts to Blob, because we already checked the instance above
    // and flow doesn't understand that event.data is type DOMString | Blob | ...
    let f = U.File.blobToFile(((event.data: any): Blob), fName);
    U.File.promptSave(f);

    return;
  }

  let msg: ClientMessage = JSON.parse(event.data),
      action = msg.clientAction;
  if (action.tag === "ShowFailureMessage") {
    Dispatcher.dispatch({
      _type: 'GOT_FAILURE',
      errorMsg: action.contents
    });

    if (isRunningTest && currentCbs) {
      // sometimes we want to test whether it errors, so it fulfills anyways!
      logDebug('Fulfilling due to server failure');
      currentCbs.fulfill(msg);
      isRunningTest = false;
    }

    return;
  }

  if (isRunningTest && (!uiTestMode || (action.tag != 'SetInitialProperties')) && currentCbs) {
    logDebug('Fulfilled server message normally');
    currentCbs.fulfill(msg);
    isRunningTest = false;
  }

  if (action.tag == "AskDecouple") {
    Dispatcher.dispatch({
      _type: 'EVAL_TRIED_TO_DECOUPLE'
    });
    return;
  }

  switch (action.tag) {
    // case 'New':
    //   if (action.payload.tag === "PayloadWorkbookSheets") {
    //     Dispatcher.dispatch({
    //       _type: 'GOT_NEW_WORKBOOKS',
    //       workbooks: action.payload.contents
    //     });
    //   }
    //   break;
    case 'NoAction':
      break;
    case 'SetInitialProperties':
      dispatchSheetUpdate(action.contents[0]);
      Dispatcher.dispatch({
        _type: 'GOT_OPEN',
        expressions: action.contents[1],
      });
      break;
    case 'UpdateSheet':
      dispatchSheetUpdate(action.contents);
      break;
    case 'ClearSheet':
      Dispatcher.dispatch({
        _type: 'CLEARED_SHEET',
        sheetId: action.contents
      });
      break;
    case 'MakeSelection':
      Dispatcher.dispatch({
        _type: 'GOT_SELECTION',
        newSelection: new ASSelection(action.contents)
      });
      break;
    case 'ShowHeaderResult':
      Dispatcher.dispatch({
        _type: 'GOT_EVAL_HEADER_RESPONSE',
        response: action.contents
      });
      break;
    case 'Find':
      // TODO
    /*
      let toClientLoc = function(x) {
        return {row:x.index[1],col:x.index[0]};
      };
      let clientLocs = action.payload.contents.map(toClientLoc);
      logDebug("GOT BACK FIND RESPONSE: " + JSON.stringify(clientLocs));
      Dispatcher.dispatch({
        _type: 'GOT_FIND',
        findLocs:clientLocs
      }); */
      break;
    case 'LoadImportedCells':
      Dispatcher.dispatch({
        _type: 'GOT_IMPORT',
        newCells: ASCell.makeCells(action.contents)
      });
      break;
  }
};

function updateIsEmpty(update: UpdateTemplate) { // same problems as makeServerMessage
  return update.newVals.length == 0 && update.oldKeys.length == 0;
}

function dispatchSheetUpdate(sheetUpdate: SheetUpdate) {
  if (!updateIsEmpty(sheetUpdate.descriptorUpdates)) {
    Dispatcher.dispatch({
      _type: 'GOT_UPDATED_RANGE_DESCRIPTORS',
      newRangeDescriptors: sheetUpdate.descriptorUpdates.newVals,
      oldRangeKeys: sheetUpdate.descriptorUpdates.oldKeys
    });
  }

  if (!updateIsEmpty(sheetUpdate.cellUpdates)) {
    Dispatcher.dispatch({
      _type: 'GOT_UPDATED_CELLS',
      newCells: ASCell.makeCells(sheetUpdate.cellUpdates.newVals),
      oldLocs: U.Location.makeLocations(sheetUpdate.cellUpdates.oldKeys)
    });
  }

  if (!updateIsEmpty(sheetUpdate.barUpdates)) {
    Dispatcher.dispatch({
      _type: 'GOT_UPDATED_BARS',
      newBars: sheetUpdate.barUpdates.newVals,
      oldBarLocs: sheetUpdate.barUpdates.oldKeys
    });
  }

  if (!updateIsEmpty(sheetUpdate.condFormatRulesUpdates)) {
    Dispatcher.dispatch({
      _type: 'GOT_UPDATED_RULES',
      newRules:
        sheetUpdate.condFormatRulesUpdates.newVals.map(
          (r) => new ASCondFormatRule(r)
        ),
      oldRuleIds: sheetUpdate.condFormatRulesUpdates.oldKeys,
    });
  }
}

wss.onopen = (evt) => {
  logDebug('WebSockets open');
};

const API = {
  sendMessageWithAction(action: any) {
    let msg = {serverAction: action};
    logDebug(`Queueing ${msg.serverAction.tag} message`);
    wss.waitForConnection((innerClient: WebSocket) => {
      logDebug(`Sending ${JSON.stringify(msg.serverAction)} message`);
      logDebug(JSON.stringify(msg));
      innerClient.send(JSON.stringify(msg));

      /* for testing */
      if ((msg.serverAction.tag === 'Acknowledge' || msg.serverAction.tag === 'Initialize') && isRunningTest && currentCbs) {
        isRunningTest = false;
        currentCbs.fulfill();
      } else if (isRunningSyncTest && currentCbs) {
        isRunningSyncTest = false;
        currentCbs.fulfill();
      }
    });
  },

  initMessage() {
    let msg = {
      tag: "Initialize",
      connUserId: SheetStateStore.getUserId(),
      connSheetId: SheetStateStore.getCurrentSheet().sheetId
    };

    API.sendMessageWithAction(msg);
  },

  ackMessage(innerClient: WebSocket) {
    let msg = { serverAction: { tag: "Acknowledge", contents: [] } };
    innerClient.send(JSON.stringify(msg));
  },

  reinitialize() {
    API.initMessage();
    API.openSheet();

    const vWindow = SheetStateStore.getViewingWindow();
    if (vWindow) {
      API.updateViewingWindow(vWindow);
    }
  },

  initialize() {
    wss.sendAck = API.ackMessage;
    wss.beforereconnect = () => { API.reinitialize(); };

    API.initMessage();
  },

  /**************************************************************************************************************************/
  /* Sending admin-related requests to the server */

  getWorkbooks() {
    // Not supporting right now (Alex 12/29)
    // let msg = U.Conversion.makeServerMessage('Get', 'PayloadList', 'WorkbookSheets');
    // API.sendMessageWithAction(msg);
  },

  close() {
    logDebug('Sending close message');
    wss.close();
  },

  export(sheet: ASSheet) {
    let msg = {
      tag: "Export",
      contents: sheet.sheetId
    };
    API.sendMessageWithAction(msg);
  },

  import(file: File) {
    // any typecast necessary because wss.send is an overloaded, untyped function...
    wss.send(((file: any): string), {binary: true});
  },

  importCSV(origin: ASIndex, lang: ASLanguage, fileName: string) {
    let msg = {
      tag: "ImportCSV",
      csvIndex: origin.obj(),
      csvLang: lang,
      csvFileName: fileName
    };

    API.sendMessageWithAction(msg);
  },

  // ************************************************************************************************************************
  /* Sending an eval request to the server */

  /* This function is called by handleEvalRequest in the eval pane */
  evaluate(origin: ASIndex, xp: ASClientExpression) {
    let msg: Evaluate = {
          tag: "Evaluate",
          contents: [{
            tag: "EvalInstruction",
            evalXp: xp,
            evalLoc: origin.obj()
          }]
        };
    API.sendMessageWithAction(msg);
  },

  evaluateHeader(expression: string, language: ASLanguage) {
    let msg = {
      tag: "EvaluateHeader",
      contents: {
        tag: "ASExpression",
        expression: expression,
        language: language
      }
    };
    API.sendMessageWithAction(msg);
  },

  // Currently not supporting (Alex 12/29)
  // evaluateRepl(xpObj: ASExpression) {
  //   let msg = U.Conversion.makeServerMessage(Constants.ServerActions.Repl, "PayloadXp", {
  //     tag: "Expression",
  //     expression: xpObj.expression,
  //     language: xpObj.language
  //   });
  //   API.sendMessageWithAction(msg);
  // },

  decouple() {
    let msg = {
      tag: "Decouple",
      contents: []
    };

    API.sendMessageWithAction(msg);
  },
  /**************************************************************************************************************************/
  /* Sending undo/redo/clear messages to the server */

  undo() {
    let msg = {
      tag: "Undo",
      contents: []
    };
    API.sendMessageWithAction(msg);
  },

  redo() {
    let msg = {
      tag: "Redo",
      contents: []
    };
    API.sendMessageWithAction(msg);
  },

  clearSheet() {
    let sid = SheetStateStore.getCurrentSheet().sheetId,
        msg: ClearSheetServer = {
          tag: "ClearSheetServer",
          contents: sid
        };
    API.sendMessageWithAction(msg);
  },

  find(findText: string) {
    // Currently not supporting -- Alex. (12/29)
    // let msg = {
    //   tag: "Find",
    //   contents: {
    //     tag: "PayloadFind",
    //     findText: findText,
    //     matchWithCase:false,
    //     matchType:0,
    //     currentSheet: "INIT_SHEET_ID",
    //     matchFullContents:false
    //   }
    // });
    // API.sendMessageWithAction(msg);
  },

  jumpSelect(range: ASRange, origin: ASIndex, isShifted: boolean, direction: Direction) {
    // Currently not supporting -- Alex. (12/29)
    // let msg = {
    //   tag: "JumpSelect",
    //   contents: {
    //     tag: "PayloadJump",
    //     isShifted: isShifted,
    //     jumpRange: U.Conversion.simpleToASRange(range),
    //     jumpOrigin: U.Conversion.simpleToASIndex(origin),
    //     jumpDirection: "D" + direction
    //   }
    // });
    // API.sendMessageWithAction(msg);
  },

  bugReport(report: string) {
    let msg = {
      tag: "BugReport",
      contents: report
    };

    API.sendMessageWithAction(msg);
  },

  deleteRange(rng: ASRange) {
    let msg: Delete = {
      tag: "Delete",
      contents: rng.obj()
    };
    API.sendMessageWithAction(msg);
  },

  getBar(bInd: BarIndex) {
    let msg = {
      tag: "GetBar",
      contents: bInd
    };
    API.sendMessageWithAction(msg);
  },

  getIsCoupled(ind: ASIndex) {
    let msg = {
      tag: "GetIsCoupled",
      contents: ind.obj()
    };
    API.sendMessageWithAction(msg);
  },

  setColumnWidth(col: number, width: number) {
    let sid = SheetStateStore.getCurrentSheet().sheetId,
        msg = {
          tag: "SetBarProp",
          contents: [
            {tag: 'BarIndex', barSheetId: sid, barType: 'ColumnType', barNumber: col},
            {tag: 'Dimension', contents: width}
          ]
        };

    API.sendMessageWithAction(msg);
  },

  setRowHeight(row: number, height: number) {
    let sid = SheetStateStore.getCurrentSheet().sheetId,
        msg = {
          tag: "SetBarProp",
          contents: [
            {tag: 'BarIndex', barSheetId: sid, barType: 'RowType', barNumber: row},
            {tag: 'Dimension', contents: height}
          ]
        };

    API.sendMessageWithAction(msg);
  },

  toggleProp(prop: ASCellProp, rng: ASRange) {
    let msg: ToggleProp = {
      tag: "ToggleProp",
      contents: [prop, rng.obj()]
    };

    API.sendMessageWithAction(msg);
  },

  // #needsrefactor should privatize, and expose only the functions that construct the prop too,
  // e.g. setTextColor.
  setProp(prop: ASCellProp, rng: ASRange) {
    let msg: SetProp = {
      tag: "SetProp",
      contents: [prop, rng.obj()]
    };

    API.sendMessageWithAction(msg);
  },

  setTextColor(contents: string, rng: ASRange) {
    let prop = {
      tag: "TextColor",
      contents: contents
    };
    this.setProp(prop, rng.obj());
  },

  setFillColor(contents: string, rng: ASRange) {
    let prop = {
      tag: "FillColor",
      contents: contents
    };
    this.setProp(prop, rng.obj());
  },

  setVAlign(contents: VAlignType, rng: ASRange) {
    let prop = {
      tag: "VAlign",
      contents: contents
    };
    this.setProp(prop, rng.obj());
  },

  setHAlign(contents: HAlignType, rng: ASRange) {
    let prop = {
      tag: "HAlign",
      contents: contents
    };
    this.setProp(prop, rng.obj());
  },

  setFontSize(contents: number, rng: ASRange) {
    let prop = {
      tag: "FontSize",
      contents: contents
    };
    this.setProp(prop, rng.obj());
  },

  setFontName(contents: string, rng: ASRange) {
    let prop = {
      tag: "FontName",
      contents: contents
    };
    this.setProp(prop, rng.obj());
  },

  setFormat(formatType: string, rng: ASRange) {
    let formatProp = {
      tag: "ValueFormat",
      formatType: formatType
    };
    this.setProp(formatProp, rng.obj());
  },

  setUrl(urlLink: string, rng: ASRange) {
    let prop = {
      tag: "URL",
      urlLink: urlLink
    };
    this.setProp(prop, rng.obj());
  },

  drag(activeRng: ASRange, dragRng: ASRange) {
    let msg = {
      tag: "Drag",
      initialRange: activeRng.obj(),
      dragRange: dragRng.obj()
    };

    API.sendMessageWithAction(msg);
  },

  copy(fromRng: ASRange, toRng: ASRange) {
    let msg = {
      tag: "Copy",
      copyFrom: fromRng.obj(),
      copyTo: toRng.obj()
    };
    API.sendMessageWithAction(msg);
  },

  cut(fromRng: ASRange, toRng: ASRange) {
    let msg = {
      tag: "Cut",
      cutFrom: fromRng.obj(),
      cutTo: toRng.obj()
    };
    API.sendMessageWithAction(msg);
  },

  pasteSimple(evalInstructions: Array<EvalInstruction>) {
    let msg = {
      tag: "Evaluate",
      contents: evalInstructions
    };

    API.sendMessageWithAction(msg);
  },

  getIndices(locs: Array<ASIndex>) {
    let msg = {
      tag: "Get",
      contents: locs.map((loc) => loc.toNaked())
    };
    API.sendMessageWithAction(msg);
  },

  repeat(sel: ASSelection) {
    // temporarily not maintaining (Alex 12/29)
    // let msg = {
    //   tag: "Repeat",
    //   contents:  {
    //     selectionRange: U.Conversion.simpleToASRange(sel.range),
    //     selectionOrigin: U.Conversion.simpleToASIndex(sel.origin)
    // });
    // API.sendMessageWithAction(msg);
  },

  insertCol(c: number) {
    let mutateType = {
      tag: "InsertCol",
      insertColNum: c
    };
    let msg = {
      tag: "MutateSheet",
      contents: mutateType
    };
    API.sendMessageWithAction(msg);
  },

  insertRow(r: number) {
    let mutateType = {
      tag: "InsertRow",
      insertRowNum: r
    };
    let msg = {
      tag: "MutateSheet",
      contents: mutateType
    };
    API.sendMessageWithAction(msg);
  },

  deleteCol(c: number) {
    let mutateType = {
      tag: "DeleteCol",
      deleteColNum: c
    };
    let msg = {
      tag: "MutateSheet",
      contents: mutateType
    };
    API.sendMessageWithAction(msg);
  },

  deleteRow(r: number) {
    let mutateType = {
      tag: "DeleteRow",
      deleteRowNum: r
    };
    let msg = {
      tag: "MutateSheet",
      contents: mutateType
    };
    API.sendMessageWithAction(msg);
  },

  dragCol(c1: number, c2: number) {
    let mutateType = {
      tag: "DragCol",
      oldColNum: c1,
      newColNum: c2
    };
    let msg = {
      tag: "MutateSheet",
      contents: mutateType
    };
    API.sendMessageWithAction(msg);
  },

  dragRow(r1: number, r2: number) {
    let mutateType = {
      tag: "DragRow",
      oldRowNum: r1,
      newRowNum: r2
    };
    let msg = {
      tag: "MutateSheet",
      contents: mutateType
    };
    API.sendMessageWithAction(msg);
  },

  // @optional mySheet
  openSheet(mySheet?: ASSheet) {
    let sheet = mySheet || SheetStateStore.getCurrentSheet(),
        msg = {
          tag: "Open",
          contents: sheet.sheetId
        };
    API.sendMessageWithAction(msg);
  },

  createSheet() {
    // Currently not supporting. (Alex 12/29)
    // let wbs = U.Conversion.makeWorkbookSheet();
    // let msg = U.Conversion.makeServerMessage(Constants.ServerActions.New,
    //   "PayloadWorkbookSheets",
    //   [wbs]);
    // API.sendMessageWithAction(msg);
  },

  createWorkbook() {
    // Not supporting now (Alex 12/29)
    // let wb = U.Conversion.makeWorkbook();
    // let msg = U.Conversion.makeServerMessage(Constants.ServerActions.New,
    //   "PayloadWB",
    //   wb);
    // API.sendMessageWithAction(msg);
  },

  updateCondFormattingRule(rule: ASCondFormatRule) {
    let msg = {
      tag: "UpdateCondFormatRules",
      contents: {
        tag: "Update",
        newVals: [rule.obj()],
        oldKeys: []
      }
    };
    API.sendMessageWithAction(msg);
  },

  removeCondFormattingRule(ruleId: string) {
    let msg = {
      tag: "UpdateCondFormatRules",
      contents: {
        tag: "Update",
        newVals: [],
        oldKeys: [ruleId]
      }
    };
    API.sendMessageWithAction(msg);
  },

  updateViewingWindow(vWindow: ASRange) {
    let msg: UpdateWindow = {
      tag: "UpdateWindow",
      contents: {
        window: vWindow.obj().range,
        sheetId: vWindow.sheetId
      }
    };
    API.sendMessageWithAction(msg);
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

export default API;
