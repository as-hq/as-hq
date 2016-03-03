/* @flow */

import type { ASAction } from '../types/Actions';

// $FlowFixMe
import { ReduceStore } from 'flux/utils';
import Immutable from 'immutable';
// $FlowFixMe
import invariant from 'invariant';
import dispatcher from '../Dispatcher';
import ASSelection from '../classes/ASSelection';
import Render from '../AS/Renderers';

import SheetStateStore from './ASSheetStateStore';

type State = Immutable.Record$Class;
const StateRecord = Immutable.Record({
  activeSelection: null,

  // XXX (anand) this variable determines whether the last fired
  // selection action should cause a scroll. This is shitty
  // flow, and should be fixed using ScrollManager.
  shouldNotScroll: false,
  lastActiveSelection: null
});

class SelectionStore extends ReduceStore<State> {
  getInitialState(): State {
    return new StateRecord();
  }

  reduce(state: State, action: ASAction): State {
    switch(action._type) {
      case 'LOGIN_SUCCESS': {
        // wait for the sheetId to be established upon login
        this.getDispatcher().waitFor([SheetStateStore.dispatcherIndex]);

        return new StateRecord({
          activeSelection: ASSelection.defaultSelection(),
          lastActiveSelection: ASSelection.defaultSelection()
        });
      }

      case 'SELECTION_CHANGED': {
        return setActiveSelection(state, action.selection)
              .set('shouldNotScroll', !! action.shouldNotScroll);
      }

      case 'API_EVALUATE': {
        return setActiveSelection(state, state.activeSelection.shift(
          action.moveDirection,
          false
        ));
      }

      default:
        return state;
    }
  }

  getActiveSelection(): ASSelection {
    const sel = this.getState().activeSelection;
    invariant(sel, 'Active selection not available for authenticated user!');
    return sel;
  }

  getLastActiveSelection(): ASSelection {
    const sel = this.getState().lastActiveSelection;
    invariant(sel, 'Last active selection not available for authenticated user!');
    return sel;
  }

  shouldScroll(): boolean {
    return ! this.getState().shouldNotScroll;
  }

}

function setActiveSelection(state: State, activeSelection: ASSelection) {
  // Render.setSelection() is for speed purposes only. Ideally we would be
  // getting the selection from this store during render, but getting the
  // variable from the store is empirically much slower than just setting
  // its value directly in the file. (Relayed from Anand -- Alex 12/9)
  Render.setSelection(activeSelection);
  const lastActiveSelection = state.activeSelection;
  return new StateRecord({
    activeSelection,
    lastActiveSelection
  });
}

export default new SelectionStore(dispatcher);
