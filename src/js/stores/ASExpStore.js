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

import Store from './ASEvaluationStore';

import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';
import ParseUtils from '../AS/ParsingUtils';
import Render from '../AS/Render';

/*
This store is for holding common data for three-way data flow between grid, textbox, and editor
*/

// TODO: Add a new datatype for constants?
let _data : {
    xpChangeOrigin : ?string,
    lastCursorPosition: ?string,
    deps: Array<NakedRange>,
    expression: string,
    language : ?ASLanguage,
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
  language: null,

  lastRef: null,
  refInsertionBypass: false,

  userIsTyping:false,

  doEditorCallback: true,
  doTextBoxCallback: true,

  clickType: null

};

const ASExpStore = Object.assign({}, BaseStore, {

  dispatcherIndex: Dispatcher.register(function (action) {
    logDebug("Exp Store detected dispatcher payload");
    switch (action._type) {
      case 'GRID_KEY_PRESSED':
      case 'EDITOR_CHANGED':
      case 'TEXTBOX_CHANGED':
        ASExpStore.updateStoreNormalTyping(action._type, action.xpStr);
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
      case 'DELETED_LOCS':
      case 'GOT_UPDATED_CELLS':
        Dispatcher.waitFor([Store.dispatcherIndex]);
        let sel = Store.getActiveSelection();

        if (sel != null) {
            let cell = Store.getCell(sel.origin.col, sel.origin.row);
            ASExpStore.updateOnBackendChange(cell);
        }
        break;
      // The spreadsheet listens to this and sets focus to grid
      case 'FETCHED_CELLS':
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
    // update deps
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

  getLanguage(): ?ASLanguage {
    return _data.language;
  },

  setLanguage(lang) {
    _data.language = lang;
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
      _data.refInsertionBypass || ParseUtils.canInsertCellRef(editor,this.getLastRef()) : false;
  },

  textBoxCanInsertRef(editor) : boolean {
    return this.getLastCursorPosition() === Constants.CursorPosition.TEXTBOX ?
      _data.refInsertionBypass || ParseUtils.canInsertCellRef(editor,this.getLastRef()) : false;
  },

  gridCanInsertRef() : boolean {
    let gridCanInsertRef = false;
    if (this.getLastCursorPosition() === Constants.CursorPosition.GRID) {
      let xp = this.getExpression(),
          lRef = this.getLastRef();
      gridCanInsertRef = lRef ?
        ParseUtils.canInsertCellRefAfterPrefix(xp.substring(0,xp.length-lRef.length)) :
        ParseUtils.canInsertCellRefAfterPrefix(xp);
    }
    return _data.refInsertionBypass || gridCanInsertRef;
  },


  /**************************************************************************************************************************/
  // Update helpers

  updateStoreNormalTyping(type, xpStr) {
    this.setUserIsTyping(true);
    this.setXpChangeOrigin(type);
    this.setExpression(xpStr);
    this.setLastRef(null); // no longer have a "last ref"
    let lang = this.getLanguage(),
        deps = Util.parseDependencies(xpStr, lang);
    logDebug("DEPS: " + JSON.stringify(deps));
    Store.setActiveCellDependencies(deps);
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
    Store.setActiveCellDependencies(Util.parseDependencies(xpStr, lang));
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
