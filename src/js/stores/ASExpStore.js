import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';

import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';

/*
This store is for holding common data for three-way data flow between grid, textbox, and editor
*/

let _data = {
  xpOrigin: null,
  deps: [],
  expression: '',
  expressionWithoutLastRef: '',
  userIsTyping:false,
  doAceCallback: false
};

dispatcherIndex: Dispatcher.register(function (action) {
  console.log("Exp Store detected dispatcher payload");
  switch (action.type) {
    case Constants.ActionTypes.GRID_KEY_PRESSED:
      ASExpStore.updateStore(Constants.xpChange.FROM_GRID, action.xpStr);
      break;
    case Constants.ActionTypes.EDITOR_CHANGED:
      ASExpStore.updateStore(Constants.xpChange.FROM_EDITOR, action.xpStr);
      break;
    case Constants.ActionTypes.TEXTBOX_CHANGED:
      ASExpStore.updateStore(Constants.xpChange.FROM_TEXTBOX, action.xpStr);
      break;
    case Constants.ActionTypes.NORMAL_SEL_CHANGED:
      break;
    case Constants.ActionTypes.PARTIAL_REF_FOUND:
      break;
    default:
      break;
  }
});

const ASExpStore = assign({}, BaseStore, {

  /**************************************************************************************************************************/
  // store getter and setter methods

  getXpOrigin(){
    return _data.xpOrigin;
  },

  setXpOrigin(xpOrigin){
    _data.xpOrigin = xpOrigin;
  },

  getExpression(){
    return _data.expression;
  },

  setExpression(xpStr){
    _data.expression = xpStr;
    // update deps
  },

  setExpressionWithoutLastRef(xpStr){
    _data.expressionWithoutLastRef = xpStr;
  },

  getDoAceCallback(){
    return _data.doAceCallback;
  },

  setDoAceCallback(bool){
    _data.doAceCallback = bool; 
  },


  /**************************************************************************************************************************/
  // Update helpers

  updateStore(type,xpStr){
    this.setXpOrigin(type);
    this.setExpression(xpStr);
    this.setExpressionWithoutLastRef(xpStr);
    this.emitChange();
  }

});


export default ASExpStore;
