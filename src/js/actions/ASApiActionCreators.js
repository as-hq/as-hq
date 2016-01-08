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
  ASIndexObject,
  ASRangeObject,

  ASSelectionObject,
  ASLanguage,
  ASExpression,
  ASValue,
  ASSheet,
  ASCellProp,
  ASCellObject,
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
  CondFormatRule,
  CondFormatCondition
} from '../types/CondFormat';

import type {
  SheetUpdate,
  CondFormatRuleUpdate,
  Update
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
    case 'AskUserToOpen':
      alert("Please refresh, and load the sheet named " + action.contents);
      break;
    case 'MakeSelection':
      Dispatcher.dispatch({
        _type: 'GOT_SELECTION',
        newSelection: action.contents
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
  }
};

function updateIsEmpty(update: Update) {
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
      oldLocs: sheetUpdate.cellUpdates.oldKeys
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
      newRules: sheetUpdate.condFormatRulesUpdates.newVals,
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
    logDebug(`Queueing ${msg.serverAction} message`);
    wss.waitForConnection((innerClient: WebSocket) => {
      logDebug(`Sending ${msg.serverAction} message`);
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
    this.initMessage();
    this.openSheet();
    this.updateViewingWindow(
      U.Conversion.rangeToASWindow(SheetStateStore.getViewingWindow().range)
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

  importCSV(origin: NakedIndex, lang: ASLanguage, fileName: string) {
    let asIndex = U.Conversion.simpleToASIndex(origin);
    let msg = {
      tag: "ImportCSV",
      csvIndex: asIndex,
      csvLang: lang,
      csvFileName: fileName
    };

    API.sendMessageWithAction(msg);
  },

  // ************************************************************************************************************************
  /* Sending an eval request to the server */

  /* This function is called by handleEvalRequest in the eval pane */
  evaluate(origin: NakedIndex, xp: ASClientExpression) {
    let asIndex = U.Conversion.simpleToASIndex(origin),
        msg: Evaluate = {
          tag: "Evaluate",
          contents: [{
            tag: "EvalInstruction",
            evalXp: xp,
            evalLoc: U.Conversion.simpleToASIndex(origin)
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

  jumpSelect(range: NakedRange, origin: NakedIndex, isShifted: boolean, direction: Direction) {
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

  deleteRange(rng: NakedRange) {
    let msg: Delete = {
      tag: "Delete",
      contents: U.Conversion.simpleToASRange(rng)
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

  getIsCoupled(ind: ASIndexObject) {
    let msg = {
      tag: "GetIsCoupled",
      contents: ind
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

  toggleProp(prop: ASCellProp, rng: NakedRange) {
    let msg: ToggleProp = {
      tag: "ToggleProp",
      contents: [prop, U.Conversion.simpleToASRange(rng)]
    };

    API.sendMessageWithAction(msg);
  },

  // #needsrefactor should privatize, and expose only the functions that construct the prop too,
  // e.g. setTextColor.
  setProp(prop: ASCellProp, rng: NakedRange) {
    let msg: SetProp = {
      tag: "SetProp",
      contents: [prop, U.Conversion.simpleToASRange(rng)]
    };

    API.sendMessageWithAction(msg);
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

  drag(activeRng: NakedRange, dragRng: NakedRange) {
    let msg = {
      tag: "Drag",
      initialRange: U.Conversion.simpleToASRange(activeRng),
      dragRange: U.Conversion.simpleToASRange(dragRng)
    };

    API.sendMessageWithAction(msg);
  },

  copy(fromRng: ASRangeObject, toRng: ASRangeObject) {
    let msg = {
      tag: "Copy",
      copyFrom: fromRng,
      copyTo: toRng
    };
    API.sendMessageWithAction(msg);
  },

  cut(fromRng: ASRangeObject, toRng: ASRangeObject) {
    let msg = {
      tag: "Cut",
      cutFrom: fromRng,
      cutTo: toRng
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

  getIndices(locs: Array<ASIndexObject>) {
    let msg = {
      tag: "Get",
      contents: locs
    };
    API.sendMessageWithAction(msg);
  },

  repeat(sel: ASSelectionObject) {
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

  updateCondFormattingRule(rule: CondFormatRule) {
    let msg = {
      tag: "UpdateCondFormatRules",
      contents: {
        tag: "Update",
        newVals: [rule],
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

  updateViewingWindow(vWindow: ASClientWindow) {
    let msg: UpdateWindow = {
      tag: "UpdateWindow",
      contents: vWindow
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
