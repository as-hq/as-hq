/* @flow */

// This store tracks the current view configurations.

import type { ASAction } from '../types/Actions';
import type { BottomPane } from '../types/State';

import Immutable from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';
import dispatcher from '../Dispatcher';
import HeaderOutputStore from './ASHeaderOutputStore';

// #flowlens
type State = any;

const StateRecord = Immutable.Record({
  isConnected: true,
  headerOpen: false,
  bottomPane: null,
  findBarOpen: false,
  findModalOpen: false
});

class ConfigurationStore extends ReduceStore<State> {
  getInitialState(): State {
    return new StateRecord();
  }

  reduce(state: State, action: ASAction): State {
    switch(action._type) {
      case 'SET_CONNECTING_STATE': {
        return state.set('isConnected', action.isConnected);
      }

      case 'HEADER_TOGGLED': {
        return state.update('headerOpen', val => !val);
      }

      case 'BOTTOM_PANE_TOGGLED': {
        const {pane} = action;
        return state.set(
          'bottomPane',
          (pane !== state.bottomPane) ? pane : null
        );
      }

      case 'HEADER_EVALUATED': {
        if (!HeaderOutputStore.isOutputEmptyInCurrentHeaderLanguage()) {
          return state.set(
            'bottomPane',
            'header_output'
          );
        }
      }

      case 'FIND_BAR_VISIBILITY_CHANGED': {
        return state.set(
          'findBarOpen',
          action.isOpen
        );
      }

      case 'FIND_MODAL_VISIBILITY_CHANGED': {
        return state.set(
          'findModalOpen',
          action.isOpen
        );
      }

      default:
        return state;
    }
  }

  isConnected(): boolean {
    return this.getState().isConnected;
  }

  isHeaderOpen(): boolean {
    return this.getState().headerOpen;
  }

  isFindBarOpen(): boolean {
    return this.getState().findBarOpen;
  }

  isFindModalOpen(): boolean {
    return this.getState().findModalOpen;
  }

  getCurrentBottomPane(): ?BottomPane {
    return this.getState().bottomPane;
  }
}

export default new ConfigurationStore(dispatcher);
