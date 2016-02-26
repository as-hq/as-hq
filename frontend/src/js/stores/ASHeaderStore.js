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
  output: null
});

const HeaderStoreRecord = Immutable.Record({
  data: Immutable.Map().withMutations(mut => {
    for (const key in Constants.Languages) {
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
      case 'HEADER_UPDATED': {
        const {language, expression} = action;
        return state.setIn(
          ['data', language, 'expression'],
          expression
        );
      }

      case 'HEADER_EVALUATED': {
        const language = state.get('currentLanguage');
        const {value, display} = action;
        const output = `${value}\n-------------------\n${display}`;
        return state.setIn(
          ['data', language, 'output'],
          output
        );
      }

      case 'NORMAL_SEL_CHANGED':
      case 'LANGUAGE_CHANGED':
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

  getCurrentOutput(): ?string {
    return this._getCurrentHeader().get('output');
  }

  _getCurrentHeader(): HeaderData {
    const state = this.getState();
    const language = state.get('currentLanguage');
    return state.getIn(['data', language]);
  }
}

export default new HeaderStore(dispatcher);
