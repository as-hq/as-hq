import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';
import Util from '../AS/Util';

/*
This store has repl interactions with backend
For example, it decides whether or not to display the backend response
Bug: If the REPL sends an "invalid" message to backend (for example, entering abc, no quotes)
  backend disconnects and nothing happens upon Ctrl Enter
--  Ritesh 10/12
*/


let replExps = {};
for (var key in Constants.Languages) {
  replExps[key]=">>> ";
}


let _data = {
  replExps: replExps,
  replShow: null,
  replSubmitLang: null,
  currentLanguage: Constants.Languages.Python
};

/* This function describes the actions of the ASReplStore upon recieving a message from Dispatcher */
dispatcherIndex: Dispatcher.register(function (action) {
    switch (action.type) {
      /* Called by Eval Pane upon leaving/changing a REPL (simply sets the expression in store) */
      case Constants.ActionTypes.REPL_LEFT:
        ASReplStore.updateReplExp(action.lang, action.value);
        break;
      /* Called after server responds to a REPL submission */
      case Constants.ActionTypes.GOT_REPL_RESP:
        ASReplStore.updateUponResponse(action.response);
        ASReplStore.emitChange();
        break;
      }
  })

const ASReplStore = assign({}, BaseStore, {

  updateReplExp(lang,value){
    console.log("In repl store, updating repl data "+ lang + " " + value);
    _data.replExps[lang] = value;
    console.log(JSON.stringify(_data.replExps));
  },

  getReplExp(lang){
    return _data.replExps[lang];
  },

  getExps(){
    return _data.replExps;
  },

  getSubmittedLanguage(){
    return _data.replSubmitLang;
  },

  updateUponResponse(resp){
    console.log("In repl store, updating response repl data "+ JSON.stringify(resp));
    let lang = resp.replLang,
        val = Util.showValue(resp.replValue, true);
    _data.replShow = this.shouldShowResponse(resp);
    _data.replSubmitLang = lang
    console.log("previous data: " +_data.replExps[lang] );
    if (_data.replShow){
      _data.replExps[lang] += "\n>>> " + val + "\n>>> ";
    }
    else this.advanceLine(lang);
  },

// @optional lang
  advanceLine(lang) {
    if (lang)
      _data.replExps[lang] += "\n>>> ";
    else
      _data.replExps[_data.currentLanguage.Server] += "\n>>> ";
  },

  setLanguage(lang) {
    _data.currentLanguage = lang;
  },

  // TODO: make fully correct (seems alright for now -- Ritesh 10/12)
  shouldShowResponse(resp){
    if (resp.replValue.tag=="NoValue"){
      return false;
    }
    return true;
  }


});


export default ASReplStore;
