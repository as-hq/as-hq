/* @flow */

// Stores the Header expressions for every language.

import type {
  ASLanguage
} from '../types/Eval';

import type {
  ASAction
} from '../types/Actions';

import Immutable from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';

import dispatcher from '../Dispatcher';

import Constants from '../Constants';
import U from '../AS/Util';


type HeaderData = Immutable.Record$Class;
type HeaderStoreData = Immutable.Record$Class;

const HeaderRecord = Immutable.Record({
  expression: '', 
  wasEvaluated: true
});

const HeaderStoreRecord = Immutable.Record({
  data: Immutable.Map().withMutations(mut => {
    for (const key in Constants.HeaderLanguages) {
      mut.set(key, new HeaderRecord());
    }
  }),
  currentLanguage: 'Python'
});

class HeaderStore extends ReduceStore<HeaderStoreData> {

  getInitialState(): HeaderStoreData {
    return new HeaderStoreRecord();
  }

  reduce(state: HeaderStoreData, action: ASAction): HeaderStoreData {
    switch (action._type) {
      case 'CLEARED_SHEET':
        return this.getInitialState();
  
      case 'HEADER_UPDATED': {
        const {language, expression} = action;
        return state.mergeIn(
          ['data', language],
          { expression, wasEvaluated: false }
        );
      }

      case 'HEADER_EVALUATED': {
        return state.mergeIn(
          ['data', this.getCurrentLanguage()],
          { wasEvaluated: true }
        );
      }

      case 'HEADER_LANGUAGE_CHANGED': {
        return state.set('currentLanguage', action.language);
      }

      case 'HEADER_DATA_RESET': {
        return state.withMutations(mut => {
          // $FlowFixMe flow literally doesn't understand this action
          action.headers.forEach(({evalHeaderLang, evalHeaderExpr}) => {
            mut.setIn(
              ['data', evalHeaderLang],
              new HeaderRecord({expression: evalHeaderExpr})
            );
          });
        });
      }

      default: {
        return state;
      }
    }
  }

  getCurrentExpression(): string {
    return this._getCurrentHeader().get('expression');
  }

  getCurrentLanguage(): ASLanguage {
    return this.getState().get('currentLanguage');
  }

  // Currently returns whether the expression in the current header (i.e. for the
  // current language) was not evaluated. 
  // #needsrefactor This should eventually count as "dirty" as long as ANY of the
  // headers (as opposed to just the current one) have an unevaluated expression. 
  isDirty(): boolean { 
    return !this._getCurrentHeader().get('wasEvaluated'); 
  }

  _getCurrentHeader(): HeaderData {
    const state = this.getState();
    const language = state.get('currentLanguage');
    return state.getIn(['data', language]);
  }
}

export default new HeaderStore(dispatcher);
