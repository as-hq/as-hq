/* @flow */

import type {
  ASClientLanguage
} from '../types/State';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
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

let _data: {
  evalHeaderExps: {[key: string]: string};
  evalHeaderSubmitLang: ?ASClientLanguage;
  evalHeaderDispMessage: ?string;
  currentLanguage: ASClientLanguage;
} = {
  evalHeaderExps: evalHeaderExps,
  evalHeaderSubmitLang: null,
  evalHeaderDispMessage: null,
  currentLanguage: Constants.Languages.Python
};

/* This function describes the actions of the ASEvalHeaderStore upon recieving a message from Dispatcher */
dispatcherIndex: Dispatcher.register(function (action) {
    switch (action._type) {
      /* Called by Eval Pane upon leaving/changing a REPL (simply sets the expression in store) */
      case 'EVAL_HEADER_CLOSED':
        ASEvalHeaderStore.updateEvalHeaderExp(action.lang, action.value);
        break;
      case 'GOT_EVAL_HEADER_RESPONSE':
        _data.evalHeaderDispMessage = ASEvalHeaderStore.makeDispMessage(action.response);
        ASEvalHeaderStore.emitChange();
        break;
      case 'GOT_OPEN':
        let xpObjs = action.expressions;
        xpObjs.forEach((xpObj) => {
          if (xpObj.language === undefined || xpObj.language === null) {
            throw new Error('Language undefined for expression');
          }
          let lang = xpObj.language,
              expr = xpObj.expression,
              uppercasedLang = lang.charAt(0).toUpperCase() + lang.slice(1);
          evalHeaderExps[uppercasedLang] = expr;
        });
        _data.evalHeaderDispMessage = ""; // don't display any message right now
        ASEvalHeaderStore.emitChange();
        break;
      }
  })

const ASEvalHeaderStore = Object.assign({}, BaseStore, {

  updateEvalHeaderExp(lang,value) {
    logDebug("In evalHeader store, updating evalHeader data " + lang + " " + value);
    _data.evalHeaderExps[lang] = value;
    logDebug(JSON.stringify(_data.evalHeaderExps));
  },

  makeDispMessage(val) {
    let message = "Header saved! ";
    switch (val.tag) {
      case "ValueError":
        message += "(Error in header code: " + val.errorMsg + ")";
        break;
      case "NoValue":
        break;
      default:
        message += "(Header code evaluated to " + JSON.stringify(((val: any): { contents: any }).contents) + ")";
        break;
    }
    return message;
  },

  getDispMessage(val) {
    return _data.evalHeaderDispMessage;
  },

  getEvalHeaderExp(lang) {
    return _data.evalHeaderExps[lang];
  },

  getExps() {
    return _data.evalHeaderExps;
  },

  getSubmittedLanguage() {
    return _data.evalHeaderSubmitLang;
  },

  setLanguage(lang) {
    _data.currentLanguage = lang;
  },
});


export default ASEvalHeaderStore;
