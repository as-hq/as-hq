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
import HeaderStore from './ASHeaderStore';
// import U from '../AS/Util';

type HeaderOutputStoreData = Immutable.Record$Class;

const HeaderOutputStoreRecord = Immutable.Record({
  data: Immutable.Map().withMutations(mut => {
    for (const key in Constants.Languages) {
      mut.set(key, "");
    }
  })
});

class HeaderOutputStore extends ReduceStore<HeaderOutputStoreData> {

  getInitialState(): HeaderOutputStoreData {
    return new HeaderOutputStoreRecord();
  }

  reduce(state: HeaderOutputStoreData, action: ASAction): HeaderOutputStoreData {
    switch (action._type) {
      case 'HEADER_EVALUATED': {
        this.__emitChange();
        const language = HeaderStore.getCurrentLanguage(); // I find this a little sketchy. (Alex 3/2)
        const {value, display} = action;
        const output = `${value || ""}\n-------------------\n${display || ""}`;
        return state.setIn(
          ['data', language],
          output
        );
      }

      case 'HEADER_DATA_RESET': {
        return state.withMutations(mut => {
          // $FlowFixMe flow literally doesn't understand this action
          action.headers.forEach(({evalHeaderLang, evalHeaderExpr}) => {
            mut.setIn(
              ['data', evalHeaderLang],
              ""
            );
          });
        });
      }

      default: {
        return state;
      }
    }
  }

  getOutputInLanguage(lang: ASLanguage): ?string {
    return this.getState().getIn(['data', lang]); 
  }

  isOutputEmptyInLanguage(lang: ASLanguage): ?string { 
    const output = this.getOutputInLanguage(lang);
    switch (lang) { 
      case 'Python': return output == "None\n-------------------\n";
      case 'R': return output == "NULL\n-------------------\n";
      default: return output == "\n-------------------\n";
    }
  }
}

export default new HeaderOutputStore(dispatcher);
