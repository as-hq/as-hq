/* @flow */

import type {
  EvalResult,
  ASLanguage
} from '../types/Eval';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import U from '../AS/Util';

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
  evalHeaderSubmitLang: ?ASLanguage;
  evalHeaderDispMessage: ?string;
  currentLanguage: ASLanguage;
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
      case 'EVAL_HEADER_UPDATED':
        ASEvalHeaderStore.updateEvalHeaderExp(action.lang, action.value);
        break;
      case 'GOT_EVAL_HEADER_RESPONSE':
        _data.evalHeaderDispMessage = ASEvalHeaderStore.makeDispMessage(action.response);
        ASEvalHeaderStore.emitChange();
        break;
      case 'GOT_OPEN':
        action.evalHeaders.forEach((evalHeader) => {
          if (evalHeader.evalHeaderLang == null) {
            throw new Error('language undefined for eval header');
          }

          let lang = evalHeader.evalHeaderLang,
              expr = evalHeader.evalHeaderExpr,
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

  makeDispMessage(res: EvalResult) {
    let val = res.resultValue, 
        message = "Header saved! ";
    if (val.tag == "CellValue") {
      let cellVal = val.contents;
      switch (cellVal.tag) {
        case "ValueError":
          message += "(Error in header code: " + cellVal.errorMsg + ")";
          break;
        case "NoValue":
          break;
        default:
          let str = JSON.stringify(((cellVal: any): { contents: any }).contents);
          message += "(Header code evaluated to " + str + ")";
          break;
      }
    } else if (val.tag == "Expanding") {
      let expVal = val.contents;
      switch (expVal.tag) {
        case "VList":
          let listVal = expVal.contents;
          switch (listVal.tag) {
            case "A":
              message += "(Header code evaluated to : [" + listVal.contents.map(U.Render.safeExtractContentsFromValue).join(',') + "])";
              break;
            default:
              message += "(Header code evaluated to a list with dimension > 1.)";
              break;
          }
          break;
        default:
          message += "(Header code evaluated to a non-list expanding value.)";
          break;
      }
    } else {
      throw "val of unknown type passed to makeDispMessage";
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
