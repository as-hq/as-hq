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


type HeaderData = {
  expression: string;
  output: ?string;
};

type StateKeys = 'data' | 'currentLanguage';
type StateValues = ASLanguage | HeaderData;
type HeaderState = Immutable.Map<StateKeys, StateValues>;

class HeaderStore extends ReduceStore<HeaderState> {

  getInitialState(): HeaderState {
    let data = {};
    for (const key in Constants.Languages) {
      data[key] = { expression: '', output: null };
    }
    return Immutable.fromJS({data, currentLanguage: 'Python'});
  }

  reduce(state: HeaderState, action: ASAction) {
    switch (action._type) {

      case 'HEADER_UPDATED': {
        const {language, expression} = action;
        return state.setIn(
          ['data', language],
          Immutable.fromJS({ expression, output: null })
        );
      }

      case 'HEADER_EVALUATED': {
        const language = state.get('currentLanguage');
        const {value, display} = action;
        const output = `${value}\n-------------------\n${display}`;
        // $FlowFixMe immutable declaration
        return state.setIn(
          ['data', language, 'output'],
          output
        );
      }

      case 'HEADER_LANGUAGE_CHANGED': {
        return state.set('currentLanguage', action.language);
      }

      case 'HEADER_DATA_RESET': {
        return state.withMutations(mutState => {
          action.headers.forEach(({evalHeaderLang, evalHeaderExpr}) => {
          // $FlowFixMe immutable declaration
            mutState.setIn(
              ['data', evalHeaderLang],
              Immutable.fromJS({ expression: evalHeaderExpr, output: null })
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
    return this._getCurrentData().get('expression');
  }

  getCurrentLanguage(): ASLanguage {
    return this.getState().get('currentLanguage');
  }

  getCurrentOutput(): ?string {
    return this._getCurrentData().get('output');
  }

  _getCurrentData(): HeaderData {
    const state = this.getState();
    const language = state.get('currentLanguage');
    return state.getIn(['data', language]);
  }
}

export default new HeaderStore(dispatcher);
