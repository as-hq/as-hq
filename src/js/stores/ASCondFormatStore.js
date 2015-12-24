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
        ASCondFormatStore._deleteRules(action.oldRuleIds);
        ASCondFormatStore._updateRules(action.newRules);
        ASCondFormatStore.emitChange();
        break;
    }
  }),

  getRules(): Array<CondFormatRule> {
    return _data.rules;
  },

  // ::ALEX:: keep the order here good
  _updateRules(rules: Array<CondFormatRule>) {
    // delete overlaps, then add all of them in
    this._deleteRules(rules.map(({condFormatRuleId}) => condFormatRuleId));
    _data.rules = _data.rules.concat(rules);
  },

  _deleteRules(ruleIds: Array<string>) {
    _data.rules = _data.rules.filter((r) => !ruleIds.includes(r.condFormatRuleId));
  }
});

export default ASCondFormatStore;
