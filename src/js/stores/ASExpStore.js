import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';

import Store from './ASEvaluationStore';

import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';
import ParseUtils from '../AS/ParsingUtils';
import Render from '../AS/Render';


/*
This store is for holding common data for three-way data flow between grid, textbox, and editor
*/

let _data = {

  xpChangeOrigin: null,
  lastCursorPosition: Constants.CursorPosition.GRID,

  deps: [],

  expression: '',
  //scellRefChanging: '',
  lastRef: null,
  refInsertionBypass: false,

  userIsTyping:false,

  doEditorCallback: true,
  doTextBoxCallback: true,

  clickType: null

};

const ASExpStore = assign({}, BaseStore, {

  dispatcherIndex: Dispatcher.register(function (action) {
    logDebug("Exp Store detected dispatcher payload");
    switch (action._type) {
      case Constants.ActionTypes.GRID_KEY_PRESSED:
      case Constants.ActionTypes.EDITOR_CHANGED:
      case Constants.ActionTypes.TEXTBOX_CHANGED:
        ASExpStore.updateStoreNormalTyping(action._type, action.xpStr);
        break;
      case Constants.ActionTypes.NORMAL_SEL_CHANGED:
        ASExpStore.updateStoreSelChange(action.xpStr);
        break;
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_GRID:
        let curXpStr = ASExpStore.getExpression(),
            lastRef = ASExpStore.getLastRef(),
            newXpStr = lastRef ?
              curXpStr.substring(0,curXpStr.length-lastRef.length) + action.excelStr:
              curXpStr + action.excelStr;
        ASExpStore.updatePartialRef(action._type,newXpStr,action.excelStr);
        break;
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_EDITOR:
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_TEXTBOX:
        ASExpStore.updatePartialRef(action._type,action.xpStr,action.excelStr);
        break;
      case Constants.ActionTypes.ESC_PRESSED:
        logDebug("Exp store found ESC");
        ASExpStore.setExpression("");
        ASExpStore.setUserIsTyping(false);
        ASExpStore.setXpChangeOrigin(action._type);
        ASExpStore.emitChange();
        break;

      // Also need to update after some "Eval"-type events
      case Constants.ActionTypes.GOT_UNDO:
      case Constants.ActionTypes.GOT_REDO:
      case Constants.ActionTypes.DELETED_LOCS:
      case Constants.ActionTypes.GOT_UPDATED_CELLS:
        Dispatcher.waitFor([Store.dispatcherIndex]);
        let sel = Store.getActiveSelection(),
            cell = Store.getCell(sel.origin.col, sel.origin.row);
        ASExpStore.updateOnBackendChange(cell);
        break;

      default:
        break;
    }
  }),

  /**************************************************************************************************************************/
  // store getter and setter methods

  getXpChangeOrigin(){
    return _data.xpChangeOrigin;
  },

  setXpChangeOrigin(xpChangeOrigin){
    _data.xpChangeOrigin = xpChangeOrigin;
  },

  getExpression(){
    return _data.expression;
  },

  setExpression(xpStr){
    _data.expression = xpStr;
    // update deps
  },

  getDoEditorCallback(){
    return _data.doEditorCallback;
  },

  setDoEditorCallback(bool){
    _data.doEditorCallback = bool;
  },

  getDoTextBoxCallback(){
    return _data.doTextBoxCallback;
  },

  setDoTextBoxCallback(bool){
    _data.doTextBoxCallback = bool;
  },

  getUserIsTyping(){
    return _data.userIsTyping;
  },

  setUserIsTyping(bool){
    _data.userIsTyping = bool;
  },

  getLastCursorPosition(){
    return _data.lastCursorPosition;
  },

  setLastCursorPosition(f){
    _data.lastCursorPosition = f;
  },


  getLastRef(){
    return _data.lastRef;
  },

  setLastRef(excel){
    _data.lastRef=excel;
  },

  getClickType(){
    return _data.clickType;
  },

  setClickType(t){
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

  editorCanInsertRef(editor){
    return this.getLastCursorPosition() === Constants.CursorPosition.EDITOR ?
      _data.refInsertionBypass || ParseUtils.canInsertCellRef(editor,this.getLastRef()) : false;
  },

  textBoxCanInsertRef(editor){
    return this.getLastCursorPosition() === Constants.CursorPosition.TEXTBOX ?
      _data.refInsertionBypass || ParseUtils.canInsertCellRef(editor,this.getLastRef()) : false;
  },

  gridCanInsertRef(){
    let gridCanInsertRef = false;
    if (this.getLastCursorPosition() === Constants.CursorPosition.GRID){
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

  updateStoreNormalTyping(type,xpStr){
    this.setUserIsTyping(true);
    this.setXpChangeOrigin(type);
    this.setExpression(xpStr);
    this.setLastRef(null); // no longer have a "last ref"
    let deps = Util.parseDependencies(xpStr);
    logDebug("DEPS: " + JSON.stringify(deps));
    Store.setActiveCellDependencies(deps);
    this.emitChange();
  },

  updateStoreSelChange(xpStr){
    this.setUserIsTyping(false);
    this.setXpChangeOrigin(Constants.ActionTypes.NORMAL_SEL_CHANGED);
    this.setExpression(xpStr);
    this.setLastRef(null); // no longer have a "last ref"
    this.emitChange();
  },

  updatePartialRef(type,xpStr,excelStr){
    this.setXpChangeOrigin(type);
    this.setLastRef(excelStr);
    this.setExpression(xpStr);
    Store.setActiveCellDependencies(Util.parseDependencies(xpStr));
    this.emitChange();
  },

  updateOnBackendChange(cell){
    // Only do these changes if the user isn't typing (has evalled)
    // Needed bc eval broadcasts to all users, but we don't want to do these things (like changing the expression) for all users
    if (!this.getUserIsTyping()) {
      if (cell !== null){
        this.setExpression(cell.cellExpression.expression);
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
