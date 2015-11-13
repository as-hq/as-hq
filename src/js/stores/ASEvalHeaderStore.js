import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';
import Util from '../AS/Util';

/*
This store has evalHeader interactions with backend
For example, it decides whether or not to display the backend response
Bug: If the REPL sends an "invalid" message to backend (for example, entering abc, no quotes)
  backend disconnects and nothing happens upon Ctrl Enter
--  Ritesh 10/12
*/


let evalHeaderExps = {};
for (var key in Constants.Languages) {
  evalHeaderExps[key] = "";
}

let _data = {
  evalHeaderExps: evalHeaderExps,
  evalHeaderShow: null,
  evalHeaderSubmitLang: null,
  currentLanguage: Constants.Languages.Python
};

/* This function describes the actions of the ASEvalHeaderStore upon recieving a message from Dispatcher */
dispatcherIndex: Dispatcher.register(function (action) {
    switch (action.type) {
      /* Called by Eval Pane upon leaving/changing a REPL (simply sets the expression in store) */
      case Constants.ActionTypes.REPL_LEFT:
        ASEvalHeaderStore.updateEvalHeaderExp(action.lang, action.value);
        break;
      }
  })

const ASEvalHeaderStore = assign({}, BaseStore, {

  updateEvalHeaderExp(lang,value){
    logDebug("In evalHeader store, updating evalHeader data "+ lang + " " + value);
    _data.evalHeaderExps[lang] = value;
    logDebug(JSON.stringify(_data.evalHeaderExps));
  },

  getEvalHeaderExp(lang){
    return _data.evalHeaderExps[lang];
  },

  getExps(){
    return _data.evalHeaderExps;
  },

  getSubmittedLanguage(){
    return _data.evalHeaderSubmitLang;
  },

  setLanguage(lang) {
    _data.currentLanguage = lang;
  },
});


export default ASEvalHeaderStore;