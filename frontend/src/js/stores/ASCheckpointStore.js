/* @flow */


import type { ASAction } from '../types/Actions';
import type { Checkpoint } from '../types/Messages';
import type { BottomPaneType } from '../types/State';

import Immutable from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';
import dispatcher from '../Dispatcher';

type State = Immutable.List<Checkpoint>;

class CheckpointStore extends ReduceStore<State> {

  getInitialState(): State {
    return new Immutable.List();
  }

  reduce(state: State, action: ASAction): State {
    switch(action._type) {
      case 'GOT_ALL_CHECKPOINTS': {
        return new Immutable.List(action.checkpoints);
      }
      default:
        return state;
    }
  }

  getAllCheckpoints(): Immutable.List<Checkpoint> {
    return this.getState();
  }
}

export default new CheckpointStore(dispatcher);
