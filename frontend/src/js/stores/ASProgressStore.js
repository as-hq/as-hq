/* @flow */

// Track progress in cells. We want to be able to show users visually that
// their computation is currently active (as opposed to dead). To that end, we
// attach a message id to requests sent to the backend. All requests have a
// message id, but we only track evaluation requests here.

// $FlowFixMe
import Immutable from 'immutable';

import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';

import type {
  ASLocation
} from '../types/Eval';

import type {
  MessageMetadata,
  MessageId,
} from '../types/Messages';

type ProgressStoreData = Immutable.Map<MessageId, MessageMetadata>;

let _waitingIds: ProgressStoreData = Immutable.Map();

const ProgressStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register(action => {
    switch (action._type) {
      case 'MARK_SENT': {
        const { locations, messageId } = action;
        const metadata = {
          locations,
          messageTimestamp: Date.now(),
        };
        _waitingIds = _waitingIds.set(messageId, metadata);
        ProgressStore.emitChange();
        break;
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
        const {messageId} = action;
        if (_waitingIds.has(messageId)) {
          _waitingIds = _waitingIds.delete(messageId);
          ProgressStore.emitChange();
        }
        break;
      }

      case 'MARK_ALL_RECEIVED': {
        _waitingIds = Immutable.Map();
        ProgressStore.emitChange();
        break;
      }
    }
  }),

  getMessagesInProgress(): Array<MessageMetadata> {
    return _waitingIds.toArray();
  },

  get(messageId: MessageId): ?MessageMetadata {
    return _waitingIds.get(messageId)
  }
});

export default ProgressStore;
