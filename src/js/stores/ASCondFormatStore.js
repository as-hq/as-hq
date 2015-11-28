/* @flow */

import type {
  CondFormatRule
} from '../types/Messages';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import Util from '../AS/Util';

let _data = {
  rules: ([]: Array<CondFormatRule>)
};

const ASCondFormatStore = Object.assign({}, BaseStore, {

  /* This function describes the actions of the ASReplStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register(function (action) {
    switch (action._type) {
      case 'GOT_UPDATED_RULES':
        ASCondFormatStore._setRules(action.rules);
        ASCondFormatStore.emitChange();
        break;
    }
  }),

  getRules(): Array<CondFormatRule> {
    return _data.rules;
  },

  _setRules(rules: Array<CondFormatRule>) {
    _data.rules = rules;
  }
});

export default ASCondFormatStore;
