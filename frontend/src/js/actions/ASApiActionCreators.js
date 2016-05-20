/* @flow */

import type {
  ASAction,
  GotFailureAction
} from '../types/Actions';

import type {
  Callback,
  Dict
} from '../types/Base';

import type {
  ASLanguage,
  ASExpression,
  ASValue,
  Sheet,
  Workbook,
  ASCellProp,
  FormatType,
  ValueFormat,
  VAlign,
  HAlign,
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
  ChangeDecimalPrecision,
  Delete,
  ToggleProp,
  Evaluate,
  EvalHeader,
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
} from '../types/State';

import shortid from 'shortid';
import invariant from 'invariant';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import {logSlack} from '../AS/Logger';

import U from '../AS/Util';

import ASCell from '../classes/ASCell';
import ASCondFormatRule from '../classes/ASCondFormatRule';
import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

import CellStore from '../stores/ASCellStore';
import WorkbookStore from '../stores/ASWorkbookStore';
import ProgressStore from '../stores/ASProgressStore';
import LoginStore from '../stores/ASLoginStore';
import GridStore from '../stores/ASGridStore';

import ProgressActions from './ASProgressActionCreators';
import HeaderActions from './ASHeaderActionCreators';

import ConfigActions from '../actions/ASConfigActionCreators';
import LoginActions from '../actions/ASLoginActionCreators';
import NotificationActions from '../actions/ASNotificationActionCreators';
import WorkbookActions from '../actions/ASWorkbookActionCreators';

const request: any = require('superagent');
import pws from '../AS/PWSInstance';


/**************************************************************************************************************************/

const { ActionTypes } = Constants;

/*
  This action creator class serves two purposes
  1) Create messages for the server and send them
  2) Take messages received from the server and send them to dispatch
*/


let testState: ({
  awaitingHook: false;
} | {
  awaitingHook: true;
  currentCbs: ASAPICallbackPair;
}) = {
  awaitingHook: false
};

let callbackStore: Dict<ASAPICallbackPair> = {};

// This function sees if we're in test mode, and if so, it sets the callbacks
// for the messageId that was generated.
// It's called when messages are sent to the server, so that the cbs can await
// a response.
function setCallbacks(messageId: string) {
  if (testState.awaitingHook) {
    callbackStore[messageId] = testState.currentCbs;
  }
}

// This function fulfills and deletes the callbacks for the specified message,
// if they're found.
// It's called when a message comes back from the server, so that we trigger the
// integration tests to continue from the current promise
function fulfillCallbacks(msg: ClientMessage) {
  const {messageId} = msg;
  const cbs = callbackStore[messageId];

  if (cbs) {
    cbs.fulfill(msg);
    delete callbackStore[messageId];
  }
}

// Same as above but for rejection
function rejectCallbacks(msg: ClientMessage) {
  const {messageId} = msg;
  const cbs = callbackStore[messageId];

  if (cbs) {
    cbs.reject(msg);
    delete callbackStore[messageId];
  }
}

let refreshDialogShown: boolean = false;


/**************************************************************************************************************************/
/*
  Initialize remote URL.
*/

function initConnection() {
  if (Constants.isRemote && !Constants.noRouter) {
    const host = Constants.getRandomHost();
    console.warn('Contacting host', host, 'for connection parameters...');
    request
      .get(Constants.getRouterUrl(host))
      .end((err, res) => {
        if (!! res && res.status == 200) {
          const { name, filePort, backendPort, staticPort } = JSON.parse(res.text);
          Constants.REMOTE_HOST = host;
          Constants.CONTAINER_ID = name;
          Constants.BACKEND_WS_PORT = backendPort;
          Constants.BACKEND_IMPORT_PORT = filePort;
          Constants.BACKEND_STATIC_PORT = staticPort;

          const url = Constants.getBackendUrl('ws', backendPort);
          pws.begin(url);
        } else {
          console._error(`Did not receive valid response from router at ${host}; trying other hosts.`);
          Constants.hosts.splice(Constants.hosts.indexOf(host), 1);
          initConnection();
        }
      });
  } else {
    const url = Constants.getBackendUrl('ws', Constants.BACKEND_WS_PORT);
    pws.begin(url);
  }
}

initConnection();

/**************************************************************************************************************************/
/*
  Set PersistentWebSocket callbacks.
*/

pws.whenReady(() => {
  pws.ondisconnect = () => {
    window.isLoggedIn = false;
    ConfigActions.setConnectedState(false);
    LoginActions.relogin();
    // Give up on progress on disconnect.
    // https://app.asana.com/0/47051356043702/84248459839482
    ProgressActions.markAllReceived();
  };

  pws.onreconnect = () => {
    ConfigActions.setConnectedState(true);
    API.openWorkbook(WorkbookStore.getCurrentWorkbookId());
  };

  pws.onmessage = (event: MessageEvent) => {
    if (event.data instanceof Blob) {
      logDebug("Received binary data from server.");
      let fName = WorkbookStore.getCurrentSheetId() + ".as";
      // #anand event.data typecasts to Blob, because we already checked the instance above
      // and flow doesn't understand that event.data is type DOMString | Blob | ...
      let f = U.File.blobToFile(((event.data: any): Blob), fName);
      U.File.promptSave(f);

      return;
    }

    const msg: ClientMessage = JSON.parse(event.data);
    const {clientAction: action} = msg;

    ProgressActions.markReceived(msg.messageId);
    NotificationActions.dismissNotification(msg.messageId);

    if (action.tag === "ShowFailureMessage") {
      Dispatcher.dispatch({
        _type: 'GOT_FAILURE',
        errorMsg: action.contents
      });

      // Clear all progress indicators if we received an Error message.
      // We currently don't have ASExecErrors tied to cells, so there's
      // currently no way to know what to clear.
      // https://app.asana.com/0/47051356043702/84248459839477
      ProgressActions.markAllReceived();

      logDebug('Fulfilling due to server failure');
      fulfillCallbacks(msg);

      return;
    }

    logDebug('Fulfilled server message normally');
    fulfillCallbacks(msg);

    switch (action.tag) {
      case 'NoAction':
        break;
      case 'SetSheetData':
        WorkbookActions.changeSheet(action.updateSheetId);
        WorkbookActions.clearSheet(action.updateSheetId);
        WorkbookActions.updateSheet(action.update);
        break;
      case 'UpdateSheet':
        WorkbookActions.updateSheet(action.contents);
        break;
      case 'ClearSheet':
        WorkbookActions.clearSheet(action.contents);
        break;
      case 'SetOpenedWorkbook':
        WorkbookActions.setOpenedWorkbook(action.contents);
        break;
      case 'SetMyWorkbooks':
        WorkbookActions.setMyWorkbooks(action.contents);
        break;
      case 'SetObjectView':
        Dispatcher.dispatch({
          _type: 'SET_OBJECT_VIEW',
          objectView: action.objectView,
          location: action.location
        });
        break;
      case 'AskDecouple':
        // #needsrefactor should use notification; currently sticking with alert box because
        // it automatically takes the focus, which is better UX. (Can be implemented with notifications
        // but not super-high priority at the moment.)
        const shouldDecouple = API.isTesting || window.confirm("You're about to decouple cells. Are you sure?");
        if (shouldDecouple) {
          API.decouple();
        }
        // Disabled because dialog is actually better UX right now, and it's faster to do that than
        // deal with focus.
        // NotificationActions.addNotification({
        //   title: "You're about to decouple cells.",
        //   message: 'Are you sure?',
        //   level: 'warning',
        //   position: 'tc',
        //   action: {
        //     label: 'OK',
        //     callback: () => API.decouple()
        //   }
        // });

        // Clear all progress indicators if we received a Decouple message.
        // The messageId sent for
        // evaluation and the id after received after decoupling are not the same,
        // and cannot be reconciled. The current workaround is to clear all progress
        // upon receiving a Decouple message.
        // https://app.asana.com/0/47051356043702/84248459839477
        ProgressActions.markAllReceived();
        break;
      case 'AskTimeout':
        const {serverActionType, timeoutMessageId} = action;
        if (Constants.UntimedActions.includes(serverActionType)) {
          // We don't track the progress of these actions.
          break;
        }

        // reconcile the timing out messageId with its locations, using ProgressStore
        const metadata = ProgressStore.get(timeoutMessageId);
        if (!! metadata) {
          const locStr = metadata
                          .locations
                          .map((loc) => loc.toExcel().toString())
                          .join(', ')
          NotificationActions.addNotification({
            uid: timeoutMessageId,
            title: 'Cancel operation',
            message: `The operation ${serverActionType} at ${locStr} is still running. Cancel?`,
            level: 'warning',
            action: {
              label: 'OK',
              callback: () => API.timeout(timeoutMessageId)
            }
          });
        }
        break;
      case 'MakeSelection':
        Dispatcher.dispatch({
          _type: 'GOT_SELECTION',
          newSelection: new ASSelection(action.contents)
        });
        break;
      case 'HandleEvaluatedHeader':
        const {headerValue, headerDisplay} = action.headerResult;
        const {evalHeaderLang, evalHeaderExpr} = action.headerContents;
        const {headerEvaluator} = action;

        HeaderActions.update(evalHeaderExpr, evalHeaderLang);
        // Don't display the output if you're not the one who updated the header; only update
        // the contents of the header.
        // Ideally we'd be comparing the current session against the session of the user
        // who sent the update; for now we're just comparing the usernames.
        if (headerEvaluator === LoginStore.getUserId()) {
          HeaderActions.setOutput(headerValue, headerDisplay);
        }
        break;
      case 'ExportCellData':
        const {exportedIndex, contents} = action;
        const idx = new ASIndex(exportedIndex);
        const blob = new Blob([contents], {type : 'application/json'});
        const f = U.File.blobToFile(blob, idx.toExcel().toString() + '_value.txt');
        U.File.promptSave(f);
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
      case 'AuthSuccess':
        const {authUserId, openedWorkbook, workbookRefs} = action;
        LoginActions.onLoginSuccess(authUserId, openedWorkbook, workbookRefs);
        const host = window.location.hostname;
        const isTest = LoginStore.getUserId === 'test_user_id';
        // Log a login success to slack if it's  not a dev/public, and not master
        if ( host !== 'master.alphasheets.com'
          && host !== 'localhost'
          && !isTest
          && !LoginStore.userIsDev()
          && !LoginStore.isPublicLogin()) {
          const slackMsg = WorkbookStore.getSheetLink(false) + '\n' + LoginStore.getUserId();
          logSlack(slackMsg, '#userlogins');
        }
        break;
      case 'AuthFailure':
        const {failureReason} = action;
        Dispatcher.dispatch({
          _type: 'LOGIN_FAILURE',
          failureReason
        });
        break;
      case 'SessionLog':
        Dispatcher.dispatch({
          _type: 'GOT_SESSION_LOG',
          sessionLog: action.sessionLog
        });
        break;
      case 'AllSessions':
        Dispatcher.dispatch({
          _type: 'GOT_ALL_SESSIONS',
          sessions: action.allSessions
        });
        break;
      default:
        break;
    }
  };
});

// *********************************************************************************************************************/
/* API */

const API_test = {
  login() {
    console.log('about to login');
    const msg = {
      tag: 'Login',
      contents: {
        tag: 'TestAuth',
        contents: []
      }
    };
    pws.send(msg, true);
    setCallbacks('auth_message_id');
  }
};

const API = {
  // #needsrefactor a stateful variable indicating whether or not the app is being tested.
  // this exists to slightly fork logic when necessary during testing. e.g. see ASCellStore.
  isTesting: false,

  sendMessageWithAction(action: any, showProgress: boolean = true) {
    const messageId = shortid.generate();
    const msg = {
      serverAction: action,
      messageId
    };

    if (showProgress) {
      ProgressActions.markSent(msg);
    }

    setCallbacks(msg.messageId);
    pws.send(msg);
  },

  // TODO (anand) for now, this function only accepts Google OAuth id tokens
  login(idToken: string) {
    const msg = {
      tag: 'Login',
      contents: {
        tag: 'GoogleAuth',
        idToken
      }
    };
    pws.send(msg, true);
  },

  loginPublicly() {
    const msg = {
      tag: 'Login',
      contents: {
        tag: 'PublicAuth',
        contents: []
      }
    };
    pws.send(msg, true);
  },

  /**************************************************************************************************************************/
  /* Sending admin-related requests to the server */

  close() {
    logDebug('Sending close message');
    pws.close();
  },

  export(sheetId: string) {
    let msg = {
      tag: "Export",
      contents: sheetId
    };
    API.sendMessageWithAction(msg);
  },

  exportCell(idx: ASIndex) {
    if (CellStore.isNonBlankCell(idx)) {
      const msg = {
        tag: 'ExportCell',
        contents: idx.obj(),
      };
      API.sendMessageWithAction(msg);
    } else {
      NotificationActions.addNotification({
        title: `No value at ${GridStore.getActiveSelection().origin.toExcel().toString()}`,
        level: 'warning',
        autoDismiss: 2,
      });
    }
  },

  import(file: File) {
    // any typecast necessary because pws.send is an overloaded, untyped function...
    pws.send(((file: any): string), {binary: true});
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

  importExcel(sheetId: string, fileName: string) {
    let msg = {
      tag: "ImportExcel",
      excelSheetId: sheetId,
      excelFileName: fileName
    };

    API.sendMessageWithAction(msg);
  },

  // ************************************************************************************************************************
  /* Sending an eval request to the server */

  evaluate(origin: ASIndex, expression: string, language: ASLanguage) {
    const msg: Evaluate = {
      tag: "Evaluate",
      contents: [{
        tag: "EvalInstruction",
        evalXp: {expression, language},
        evalLoc: origin.obj()
      }],
    };
    API.sendMessageWithAction(msg);
  },

  evaluateHeader(expression: string, language: ASLanguage) {
    const wid = WorkbookStore.getCurrentWorkbookId();
    const msg = {
      tag: "EvaluateHeader",
      contents: {
        tag: "EvalHeader",
        evalHeaderWorkbookId: wid,
        evalHeaderExpr: expression,
        evalHeaderLang: language
      }
    };
    API.sendMessageWithAction(msg);
  },

  reEval() {
    const msg = {
      tag: "ReEval",
      contents: WorkbookStore.getCurrentSheetId()
    };
    API.sendMessageWithAction(msg);
  },

  setLanguagesInRange(language: ASLanguage, range: ASRange) {
    let action = {
          tag: "SetLanguagesInRange",
          contents: [language, range.obj()],
        };
    API.sendMessageWithAction(action);
  },

  getObjectView(index: ASIndex) {
    const action = {
      tag: "GetObjectView",
      contents: index.obj()
    };
    API.sendMessageWithAction(action, false);
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

  timeout(messageId: string) {
    const msg = {
      tag: "Timeout",
      contents: messageId
    };
    ProgressActions.markReceived(messageId);
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
    let sid = WorkbookStore.getCurrentSheetId(),
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
    let sid = WorkbookStore.getCurrentSheetId(),
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
    API.setProp(prop, rng);
  },

  setFillColor(contents: string, rng: ASRange) {
    let prop = {
      tag: "FillColor",
      contents: contents
    };
    API.setProp(prop, rng);
  },

  setVAlign(contents: VAlignType, rng: ASRange) {
    let prop: VAlign = {
      tag: "VAlign",
      contents: contents
    };
    API.setProp(prop, rng);
  },

  setHAlign(contents: HAlignType, rng: ASRange) {
    let prop: HAlign = {
      tag: "HAlign",
      contents: contents
    };
    API.setProp(prop, rng);
  },

  setFontSize(contents: number, rng: ASRange) {
    let prop = {
      tag: "FontSize",
      contents: contents
    };
    API.setProp(prop, rng);
  },

  setFontName(contents: string, rng: ASRange) {
    let prop = {
      tag: "FontName",
      contents: contents
    };
    API.setProp(prop, rng);
  },

  setFormat(formatType: FormatType, rng: ASRange) {
    let formatProp: ValueFormat = {
      tag: 'ValueFormat',
      valFormat: {tag: 'Format', formatType: formatType, numDecimals: null}
    };
    API.setProp(formatProp, rng);
  },

  toggleFormat(formatType: FormatType, rng: ASRange) {
    const formatProp: ValueFormat = {
      tag: 'ValueFormat',
      valFormat: {tag: 'Format', formatType: formatType, numDecimals: null}
    };
    API.toggleProp(formatProp, rng);
  },

  handleChangeDecimalPrecision(i: number, rng: ASRange) {
    let msg: ChangeDecimalPrecision = {
      tag: "ChangeDecimalPrecision",
      contents: [i, rng.obj()]
    };
    API.sendMessageWithAction(msg);
  },

  setUrl(urlLink: string, rng: ASRange) {
    let prop = {
      tag: "URL",
      urlLink: urlLink
    };
    API.setProp(prop, rng);
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
      contents: locs.map((loc) => loc.obj())
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

  clearSheet() {
    let sid = WorkbookStore.getCurrentSheetId(),
        msg: ClearSheetServer = {
          tag: "ClearSheetServer",
          contents: sid
        };
    API.sendMessageWithAction(msg);
  },

  // When a user receives a link to a sheet, she "acquires" that
  // sheet into her list of sheets. "Acquired" sheets are distinguished
  // from sheets she created.
  acquireSheet(sheetId: string) {
    const msg = {
      tag: "AcquireSheet",
      contents: sheetId
    };
    API.sendMessageWithAction(msg);
  },

  openSheet(sheetId: string) {
    const msg = {
      tag: "OpenSheet",
      contents: sheetId
    };
    API.sendMessageWithAction(msg);
  },

  openWorkbook(workbookId: string) {
    const msg = {
      tag: "OpenWorkbook",
      contents: workbookId
    };
    API.sendMessageWithAction(msg);
  },

  newSheet(name: string) {
    const msg = {
      tag: "NewSheet",
      contents: name,
    };
    API.sendMessageWithAction(msg);
  },

  newWorkbook(name: string) {
    const msg = {
      tag: "NewWorkbook",
      contents: name,
    };
    API.sendMessageWithAction(msg);
  },

  cloneSheet(sheetId: string) {
    const msg = {
      tag: "CloneSheet",
      contents: sheetId
    };
    API.sendMessageWithAction(msg);
  },

  renameSheet(renameSheetId: string, newSheetName: string) {
    const msg = {
      tag: 'RenameSheet',
      renameSheetId,
      newSheetName
    };
    API.sendMessageWithAction(msg);
  },

  deleteSheet(sheetId: string) {
    const msg = {
      tag: 'DeleteSheet',
      contents: sheetId,
    };
    API.sendMessageWithAction(msg);
  },

  getOpenedWorkbook() {
    const msg = {
      tag: "GetOpenedWorkbook",
      contents: [],
    };
    API.sendMessageWithAction(msg);
  },

  getMyWorkbooks() {
    const msg = {
      tag: "GetMyWorkbooks",
      contents: [],
    };
    API.sendMessageWithAction(msg);
  },

  updateCondFormattingRule(rule: ASCondFormatRule) {
    let msg = {
      tag: "UpdateCondFormatRules",
      newRules: [rule.obj()],
      oldRuleIds: []
    };
    API.sendMessageWithAction(msg);
  },

  removeCondFormattingRule(ruleId: string) {
    let msg = {
      tag: "UpdateCondFormatRules",
      newRules: [],
      oldRuleIds: [ruleId]
    };
    API.sendMessageWithAction(msg);
  },

  updateViewingWindow(vWindow: ASRange) {
    // let msg: UpdateWindow = {
    //   tag: "UpdateWindow",
    //   contents: {
    //     window: vWindow.obj().range,
    //     sheetId: vWindow.sheetId
    //   }
    // };
    // API.sendMessageWithAction(msg);
  },

  togglePauseMode() {
    const msg = {
      tag: "TogglePauseMode",
      contents: WorkbookStore.getCurrentSheetId()
    };
    API.sendMessageWithAction(msg);
  },

  /**************************************************************************************************************************/
  // Logging

  // Get all logs data for a session
  getSessionLogs(logUserId: string, logSessionId: string) {
    const msg = {
      tag: 'GetSessionLogs',
      contents: {
        logUserId,
        logSessionId
      }
    };
    API.sendMessageWithAction(msg);
  },

  // Send a pre-flight request to stop logging now that we're debugging
  startDebuggingLog() {
    const msg = {
      tag: "StartDebuggingLog",
      contents: []
    };
    API.sendMessageWithAction(msg);
  },

  // Gets all sessions
  getAllSessions() {
    const msg = {
      tag: "GetAllSessions",
      contents: []
    };
    API.sendMessageWithAction(msg);
  },

  logAction(msg: string) {
    pws.send(msg);
  },

  /**************************************************************************************************************************/
  /* Testing */

  withWS<A>(fn: (givenPws: WebSocket) => A): A {
    return fn(pws);
  },

  test(f: Callback, cbs: ASAPICallbackPair) {
    /*  NOTE: this function is only to be used on an (f: Callback) which is
        one API action that will solicit *exactly one* response from the backend
      */

    testState = {
      awaitingHook: true,
      currentCbs: cbs
    };

    f();

    testState = { awaitingHook: false };
  }
};

export { API_test };
export default API;
