import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';
import API from '../actions/ASApiActionCreators';
import Converter from '../AS/Converter';
import Util from '../AS/Util';


let replExps = {};
for (var key in Constants.Languages) {
  replExps[key]=">>> ";
}


let _data = {
  replExps: replExps,
  replShow: null,
  replSubmitLang: null
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
    let lang = resp.lang,
        val = Util.showValue(resp.langValue, true);
    _data.replShow = this.shouldShowResponse(val);
    _data.replSubmitLang = lang
    console.log("previous data: " +_data.replExps[lang] );
    if (_data.replShow){
      _data.replExps[_data.replSubmitLang] += "\n>>> " + val + "\n>>> ";
    }
    else{
      _data.replExps[_data.replSubmitLang] += "\n>>> ";
    }
  },

  // TODO: implement based on backend
  shouldShowResponse(resp){
    return true;
  }


});


export default ASReplStore;
