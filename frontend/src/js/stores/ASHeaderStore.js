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
  expression: ''
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
        return state.setIn(
          ['data', language, 'expression'],
          expression
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

  _getCurrentHeader(): HeaderData {
    const state = this.getState();
    const language = state.get('currentLanguage');
    return state.getIn(['data', language]);
  }
}

export default new HeaderStore(dispatcher);
