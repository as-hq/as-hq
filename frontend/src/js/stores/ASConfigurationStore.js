/* @flow */

// This store tracks the current view configurations.

import type { ASAction } from '../types/Actions';
import type { BottomPane } from '../types/State';

import Immutable from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';
import dispatcher from '../Dispatcher';

import CellStore from './ASCellStore';
import GridStore from './ASGridStore';
import HeaderOutputStore from './ASHeaderOutputStore';

// #flowlens
type State = any;

const StateRecord = Immutable.Record({
  isConnected: true,
  headerOpen: false,
  bottomPane: null,
  findBarOpen: false,
  findModalOpen: false,
  sheetLoading: false
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

      case 'API_OPENING_SHEET': {
        return state.set('sheetLoading', true);
      }

      case 'SHEET_UPDATED': {
        return state.set('sheetLoading', false);
      }

      case 'SELECTION_CHANGED': {
        const {origin, range} = action.selection;

        const cell = CellStore.getCell(origin);
        if (cell != null && cell.hasOutput()) {
          return state.set('bottomPane', 'cell_output');
        } else {
          const cells = CellStore.getCells(range);
          if (cells.some((cell) => cell != null && cell.hasError())) {
            return state.set('bottomPane', 'errors');
          } else {
            return state.set('bottomPane', null);
          }
        }
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

  isSheetLoading(): boolean {
    return this.getState().sheetLoading;
  }
}

export default new ConfigurationStore(dispatcher);
