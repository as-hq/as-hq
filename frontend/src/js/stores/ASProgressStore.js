/* @flow */

// Track progress in cells. We want to be able to show users visually that
// their computation is currently active (as opposed to dead). To that end, we
// attach a message id to requests sent to the backend. All requests have a
// message id, but we only track evaluation requests here.

import Immutable from 'immutable';
// $FlowFixMe
import { MapStore } from 'flux/utils';

import dispatcher from '../Dispatcher';

import type {
  ASLocation
} from '../types/Eval';

import type {
  MessageMetadata,
  MessageId,
} from '../types/Messages';

import type {
  ASAction
} from '../types/Actions';

type ProgressState = Immutable.Map<MessageId, MessageMetadata>;

class ProgressStore extends MapStore<MessageId, MessageMetadata> {

  reduce(state: ProgressState, action: ASAction): ProgressState {
    switch (action._type) {
      case 'RESET':
        return Immutable.Map();
      case 'MARK_SENT': {
        const { locations, messageId } = action;
        const metadata = {
          locations,
          messageTimestamp: Date.now(),
        };
        return state.set(messageId, metadata);
      }

      case 'MARK_RECEIVED': {
        // The user might change the value of a cell before its evaluation has
        // been returned.
        //
        // 1 <---- request ----> 1
        //      2 <---- request ----> 2
        //                            ^ mark finished here
        //
        // In this case, we're fine, since multiple messageId's will share the
        // same locations. Then, we've only deleted the in-progress locations
        // corresponding to one particular message, and the overlapping are still going.
        // ________
        // | M1 ___|____
        // |___|___| M2 |
        //     |________| <-- mark received M2, but intersect(M1, M2) are still in progress
        //
        const { messageId } = action;
        return state.delete(messageId);
      }

      case 'MARK_ALL_RECEIVED': {
        return Immutable.Map();
      }

      default: {
        return state;
      }
    }
  }

  getMessagesInProgress(): Array<MessageMetadata> {
    return this.getState().toArray();
  }
}

export default new ProgressStore(dispatcher);
