/* @flow */

import type {
  NakedRange,
  ASCell,
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

/*
This store is for holding common data for three-way data flow between grid, textbox, and editor
*/

// TODO: Add a new datatype for constants?
let _data : {
  xpChangeOrigin : ?string,
  lastCursorPosition: ?string,
  deps: Array<NakedRange>,
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
        logDebug("Exp store found ESC");
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
    return this.getLastCursorPosition() === Constants.CursorPosition.EDITOR ?
      _data.refInsertionBypass || Util.Parsing.canInsertCellRef(editor,this.getLastRef()) : false;
  },

  textBoxCanInsertRef(editor) : boolean {
    return this.getLastCursorPosition() === Constants.CursorPosition.TEXTBOX ?
      _data.refInsertionBypass || Util.Parsing.canInsertCellRef(editor,this.getLastRef()) : false;
  },

  gridCanInsertRef() : boolean {
    let gridCanInsertRef = false;
    if (this.getLastCursorPosition() === Constants.CursorPosition.GRID) {
      let xp = this.getExpression(),
          lRef = this.getLastRef();
      gridCanInsertRef = lRef ?
        Util.Parsing.canInsertCellRefAfterPrefix(xp.substring(0,xp.length-lRef.length)) :
        Util.Parsing.canInsertCellRefAfterPrefix(xp);
    }
    return _data.refInsertionBypass || gridCanInsertRef;
  },


  /**************************************************************************************************************************/
  // Update helpers

  toggleLanguage(lang: ASLanguage) {
    this.setLanguage(lang);
    this.setDefaultLanguage(lang);
  },

  // Checks if you're newly adding text to a cell with a % in it.
  shouldHandlePercentFormat() {
    let percentProp = {tag: "ValueFormat", formatType: 'Percentage'},
        wasTyping = this.getUserIsTyping(),
        cell = CellStore.getActiveCell();
    return (!wasTyping && cell != undefined && Util.Cell.cellHasProp(percentProp, cell));
  },

  updateStoreNormalTyping(type, xpStr, cursorPos) {
    this.setXpChangeOrigin(type);
    this.setExpression(xpStr);
    this.setCursorPos(cursorPos);
    this.setUserIsTyping(true);
    this.setLastRef(null); // no longer have a "last ref"
    let lang = this.getLanguage(),
        deps = Util.Parsing.parseDependencies(xpStr, lang);
    logDebug("DEPS: " + JSON.stringify(deps));
    CellStore.setActiveCellDependencies(deps);
    this.emitChange();
  },

  updateStoreSelChange(xpStr : string) {
    this.setUserIsTyping(false);
    this.setXpChangeOrigin(Constants.ActionTypes.NORMAL_SEL_CHANGED);
    this.setExpression(xpStr);
    this.setLastRef(null); // no longer have a "last ref"
    this.emitChange();
  },

  updatePartialRef(type : string, xpStr : string, excelStr : string) {
    this.setXpChangeOrigin(type);
    this.setLastRef(excelStr);
    this.setExpression(xpStr);
    let lang = this.getLanguage();
    CellStore.setActiveCellDependencies(Util.Parsing.parseDependencies(xpStr, lang));
    this.emitChange();
  },

  updateOnBackendChange(cell : ?ASCell) {
    // Only do these changes if the user isn't typing (has evalled)
    // Needed bc eval broadcasts to all users, but we don't want to do these things (like changing the expression) for all users
    if (!this.getUserIsTyping()) {
      if (cell != null) {
        if (cell.cellExpression != null) {
          this.setExpression(cell.cellExpression.expression);
        }
      } else {
        this.setExpression('');
      }
      this.setUserIsTyping(false);
      this.setXpChangeOrigin(Constants.ActionTypes.BACKEND_UPDATED_AND_CELLS_CHANGED);
      this.setLastRef(null);
      this.emitChange();
    }
  }

});


export default ASExpStore;
