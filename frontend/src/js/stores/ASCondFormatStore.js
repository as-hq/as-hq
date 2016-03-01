/* @flow */

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import Util from '../AS/Util';

import {Just} from '../AS/Maybe';

import ASRange from '../classes/ASRange';

import ASCondFormatRule from '../classes/ASCondFormatRule';

let _data = {
  rules: ([]: Array<ASCondFormatRule>)
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
      case 'RESET':
        _data.rules = [];
        break;
    }
  }),

  getRules(): Array<ASCondFormatRule> {
    return _data.rules;
  },

  getRulesApplyingToRange(rng: ASRange): Array<ASCondFormatRule> {
    return ASCondFormatStore.getRules().filter(
      (rule) => rule.appliesTo(rng)
    )
  },

  getRuleById(id: string): ?ASCondFormatRule {
    return ASCondFormatStore.getRules().filter(
      ({condFormatRuleId: rid}) => id === rid
    )[0];
  },

  withRuleById<T>(id: string, cb: (val: ASCondFormatRule) => T): ?T {
    return Just(ASCondFormatStore.getRuleById(id)).fmap(cb).out();
  },

  // #needsrefactor yes Michael I know I'm using indices etc. etc.
  _updateRules(rules: Array<ASCondFormatRule>) {
    rules.forEach((r) => {
      let rIndex = _data.rules.map(({condFormatRuleId}) => condFormatRuleId).indexOf(r.condFormatRuleId);
      if (rIndex >= 0) {
        _data.rules[rIndex] = r;
      } else {
        _data.rules.push(r);
      }
    });
  },

  _deleteRules(ruleIds: Array<string>) {
    _data.rules = _data.rules.filter((r) => !ruleIds.includes(r.condFormatRuleId));
  }
});

export default ASCondFormatStore;
