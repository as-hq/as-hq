/* @flow */

// Stores the Header expressions for every language.

import type {
  EvalResult,
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


type HeaderData = Immutable.Record & {
  expression: string;
  output: ?string;
};

type HeaderStoreData = Immutable.Record & {
  data: Immutable.Map<ASLanguage, HeaderData>;
  currentLanguage: ASLanguage;
};

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
    // $FlowFixMe immutable declaration
    return new HeaderStoreRecord();
  }

  reduce(state: HeaderStoreData, action: ASAction): HeaderStoreData {
    switch (action._type) {
      case 'HEADER_UPDATED': {
        const {language, expression} = action;
        // $FlowFixMe immutable declaration
        return state.setIn(
          ['data', language, 'expression'],
          expression
        );
      }

      case 'HEADER_EVALUATED': {
        // $FlowFixMe immutable declaration
        const language = state.get('currentLanguage');
        const {value, display} = action;
        const output = `${value}\n-------------------\n${display}`;
        // $FlowFixMe immutable declaration
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
          // $FlowFixMe immutable declaration
        return state.withMutations(mut => {
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
