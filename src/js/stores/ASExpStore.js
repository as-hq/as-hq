/* @flow */

import type {
  ASLanguage
} from '../types/Eval';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';

import CellStore from './ASCellStore';
import SheetStateStore from './ASSheetStateStore';
import SelectionStore from './ASSelectionStore';

import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';
import Render from '../AS/Renderers';

import ASCell from '../classes/ASCell';
import ASRange from '../classes/ASRange';

/*
This store is for holding common data for three-way data flow between grid, textbox, and editor
*/

// TODO: Add a new datatype for constants?
let _data : {
  xpChangeOrigin : ?string,
  lastCursorPosition: ?string,
  deps: Array<ASRange>,
  expression: string,
  language : ASLanguage,
  defaultLanguage: ASLanguage,
  cursorPos: ?number, // currently only relevant for grid
  lastRef : ?string,
  refInsertionBypass: boolean,
  userIsTyping: boolean,
  doEditorCallback: boolean,
  doTextBoxCallback: boolean,
  clickType: ?string
} =  {

  xpChangeOrigin: null,
  lastCursorPosition: Constants.CursorPosition.GRID,

  deps: [],

  expression: '',
  language: Constants.Languages.Excel,
  defaultLanguage: Constants.Languages.Excel,
  cursorPos: null,

  lastRef: null,
  refInsertionBypass: false,

  userIsTyping: false,

  doEditorCallback: true,
  doTextBoxCallback: true,

  clickType: null

};

const ASExpStore = Object.assign({}, BaseStore, {

  dispatcherIndex: Dispatcher.register(function (action) {
    logDebug("Exp Store detected dispatcher payload");
    switch (action._type) {
      case 'EDITOR_CHANGED':
      case 'TEXTBOX_CHANGED':
        ASExpStore.updateStoreNormalTyping(action._type, action.xpStr);
        break;
      case 'GRID_KEY_PRESSED':
        ASExpStore.updateStoreNormalTyping(action._type, action.xpStr, action.cursorPos);
        break;
      case 'NORMAL_SEL_CHANGED':
        ASExpStore.updateStoreSelChange(action.xpStr);
        break;
      case 'PARTIAL_REF_CHANGE_WITH_GRID':
        let curXpStr = ASExpStore.getExpression(),
            lastRef = ASExpStore.getLastRef(),
            newXpStr = lastRef ?
              curXpStr.substring(0,curXpStr.length-lastRef.length) + action.excelStr:
              curXpStr + action.excelStr;
        ASExpStore.updatePartialRef(action._type, newXpStr, action.excelStr);
        break;
      case 'PARTIAL_REF_CHANGE_WITH_EDITOR':
      case 'PARTIAL_REF_CHANGE_WITH_TEXTBOX':
        ASExpStore.updatePartialRef(action._type,action.xpStr,action.excelStr);
        break;
      case 'ESC_PRESSED':
        ASExpStore.setExpression("");
        ASExpStore.setUserIsTyping(false);
        ASExpStore.setXpChangeOrigin(action._type);
        ASExpStore.emitChange();
        break;
      // Also need to update after some "Eval"-type events
      case 'GOT_UNDO':
      case 'GOT_REDO':
      case 'GOT_UPDATED_CELLS':
        Dispatcher.waitFor([CellStore.dispatcherIndex]);
        SelectionStore.withActiveSelection(({origin}) => {
          let cell = CellStore.getCell(origin);
          ASExpStore.updateOnBackendChange(cell);
        });
        break;
      // This action is called when the toolbar's language picker or a shortcut toggles a language.
      // In this case, we want the default language to reflect this user change, in addition to the current language.
      case 'LANGUAGE_TOGGLED':
        ASExpStore.setLanguage(action.lang);
        ASExpStore.setDefaultLanguage(action.lang);
        ASExpStore.setXpChangeOrigin(action._type);
        ASExpStore.emitChange();
        break;
      default:
        break;
    }
  }),

  /**************************************************************************************************************************/
  // store getter and setter methods

  getXpChangeOrigin() {
    return _data.xpChangeOrigin;
  },

  setXpChangeOrigin(xpChangeOrigin : ?string) {
    _data.xpChangeOrigin = xpChangeOrigin;
  },

  getExpression() {
    return _data.expression;
  },

  setExpression(xpStr : string) {
    _data.expression = xpStr;
  },

  getCursorPos() {
    return _data.cursorPos;
  },

  setCursorPos(pos: ?number) {
    _data.cursorPos = pos;
  },

  getDoEditorCallback() {
    return _data.doEditorCallback;
  },

  setDoEditorCallback(bool) {
    _data.doEditorCallback = bool;
  },

  getDoTextBoxCallback() {
    return _data.doTextBoxCallback;
  },

  setDoTextBoxCallback(bool: boolean) {
    _data.doTextBoxCallback = bool;
  },

  getUserIsTyping() {
    return _data.userIsTyping;
  },

  setUserIsTyping(bool : boolean) {
    _data.userIsTyping = bool;
  },

  getLastCursorPosition() {
    return _data.lastCursorPosition;
  },

  setLastCursorPosition(f : ?string) {
    _data.lastCursorPosition = f;
  },

  getLastRef(): ?string {
    return _data.lastRef;
  },

  setLastRef(excel: ?string) {
    _data.lastRef=excel;
  },

  getLanguage(): ASLanguage {
    return _data.language;
  },

  setLanguage(lang) {
    _data.language = lang;
  },

  getDefaultLanguage(): ASLanguage {
    return _data.defaultLanguage;
  },

  setDefaultLanguage(lang) {
    _data.defaultLanguage = lang;
  },

  getClickType() : ?string {
    return _data.clickType;
  },

  setClickType(t : ?string) {
    _data.clickType = t;
    Render.setShouldRenderSquareBox((t === Constants.ClickType.CLICK));
  },

  enableRefInsertionBypass() {
    logDebug('Enabling bypass');
    _data.refInsertionBypass = true;
  },

  disableRefInsertionBypass() {
    logDebug('Disabling bypass');
    _data.refInsertionBypass = false;
  },

  /**************************************************************************************************************************/
  // Inserting ref helpers

  editorCanInsertRef(editor) : boolean {
    if ((ASExpStore.getLastCursorPosition() === Constants.CursorPosition.EDITOR)) {
      if (_data.refInsertionBypass) {
        return true;
      }

      let lastRef = ASExpStore.getLastRef();
      if (lastRef != null) {
        return Util.Parsing.canInsertCellRef(editor, lastRef);
      }
    }
    return false;
  },

  textBoxCanInsertRef(editor) : boolean {
    if ((ASExpStore.getLastCursorPosition() === Constants.CursorPosition.TEXTBOX)) {
      if (_data.refInsertionBypass) {
        return true;
      }

      let lastRef = ASExpStore.getLastRef();
      if (lastRef != null) {
        return Util.Parsing.canInsertCellRef(editor, lastRef);
      }
    }
    return false;
  },

  gridCanInsertRef() : boolean {
    let gridCanInsertRef = false;
    if (ASExpStore.getLastCursorPosition() === Constants.CursorPosition.GRID) {
      let xp = ASExpStore.getExpression(),
          lRef = ASExpStore.getLastRef();
      gridCanInsertRef = lRef ?
        Util.Parsing.canInsertCellRefAfterPrefix(xp.substring(0,xp.length-lRef.length)) :
        Util.Parsing.canInsertCellRefAfterPrefix(xp);
    }
    return _data.refInsertionBypass || gridCanInsertRef;
  },


  /**************************************************************************************************************************/
  // Update helpers

  // Checks if you're newly adding text to a cell with a % in it.
  shouldHandlePercentFormat() {
    let percentProp = {tag: "ValueFormat", formatType: 'Percentage'},
        wasTyping = ASExpStore.getUserIsTyping(),
        cell = CellStore.getActiveCell();
    return (!wasTyping && cell != undefined && cell.hasProp(percentProp));
  },

  updateStoreNormalTyping(type, xpStr, cursorPos) {
    ASExpStore.setXpChangeOrigin(type);
    ASExpStore.setExpression(xpStr);
    ASExpStore.setCursorPos(cursorPos);
    ASExpStore.setUserIsTyping(true);
    ASExpStore.setLastRef(null); // no longer have a "last ref"
    let lang = ASExpStore.getLanguage(),
        deps = Util.Parsing.parseDependencies(xpStr, lang);
    logDebug("DEPS: " + JSON.stringify(deps));
    CellStore.setActiveCellDependencies(deps);
    ASExpStore.emitChange();
  },

  updateStoreSelChange(xpStr : string) {
    ASExpStore.setUserIsTyping(false);
    ASExpStore.setXpChangeOrigin(Constants.ActionTypes.NORMAL_SEL_CHANGED);
    ASExpStore.setExpression(xpStr);
    ASExpStore.setLastRef(null); // no longer have a "last ref"
    ASExpStore.emitChange();
  },

  updatePartialRef(type : string, xpStr : string, excelStr : string) {
    ASExpStore.setXpChangeOrigin(type);
    ASExpStore.setLastRef(excelStr);
    ASExpStore.setExpression(xpStr);
    let lang = ASExpStore.getLanguage();
    CellStore.setActiveCellDependencies(Util.Parsing.parseDependencies(xpStr, lang));
    ASExpStore.emitChange();
  },

  updateOnBackendChange(cell : ?ASCell) {
    // Only do these changes if the user isn't typing (has evalled)
    // Needed bc eval broadcasts to all users, but we don't want to do these things (like changing the expression) for all users
    if (!ASExpStore.getUserIsTyping()) {
      if (cell != null) {
        if (cell.cellExpression != null) {
          ASExpStore.setExpression(cell.cellExpression.expression);
        }
      } else {
        ASExpStore.setExpression('');
      }
      ASExpStore.setUserIsTyping(false);
      ASExpStore.setXpChangeOrigin(Constants.ActionTypes.BACKEND_UPDATED_AND_CELLS_CHANGED);
      ASExpStore.setLastRef(null);
      ASExpStore.emitChange();
    }
  }

});


export default ASExpStore;
