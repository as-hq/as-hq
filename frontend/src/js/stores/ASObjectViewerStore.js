/* @flow */

import {fromJS, Map} from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';

import dispatcher from '../Dispatcher';

import type {
  ASLocation
} from '../types/Eval';

import ASIndex from '../classes/ASIndex';
import ASCell from '../classes/ASCell';

import type {
  MessageMetadata,
  MessageId,
} from '../types/Messages';

import type {
  ASAction
} from '../types/Actions';

import type {
  CellUpdate
} from '../types/Updates';


type ObjectView = {
  isDirty: boolean;
  content: string;
};

// The state is a Map<ASIndex, ObjectView>, which is just a cache
// of object views and whether they're dirty or not
type State = any;

class ObjectViewerStore extends ReduceStore<State> {

  getInitialState(): State {
    return new Map();
  }

  reduce(state: State, action: ASAction): State {
    switch (action._type) {
      case 'SHEET_UPDATED': {
        return markOutdated(state, action.update.cellUpdates);
      }
      case 'SET_OBJECT_VIEW': {
        const content = action.objectView;
        const view = {dirty: false, content};
        const loc = fromJS(ASIndex.fromNaked(action.location.index));
        return this.getState().set(loc, view);
      }
      default: {
        return state;
      }
    }
  }

  getObjectViewAt(idx: ASIndex): ?string {
    const loc = fromJS(idx);
    if (this.getState().has(loc)) {
      return this.getState().get(fromJS(idx)).content;
    } else return null;
  }

  shouldRecomputeObjectViewAt(idx: ASIndex): boolean {
    const loc = fromJS(idx);
    if (!this.getState().has(loc)) {
      return true;
    } else return this.getState().get(loc).dirty;
  }

}

function markOutdated(state: State, update: CellUpdate): State {
  let state_ = state;
  const updater = (view) => {
    view.dirty = true;
    return view
  };
  ASCell.makeCells(update.newVals).forEach((cell) => {
    const loc = fromJS(cell.location);
    if (state.has(loc)) {
      state_ = state.update(loc, updater);
    }
  });
  return state_;
}

export default new ObjectViewerStore(dispatcher);
