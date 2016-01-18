/* @flow */

// Track progress in cells. We want to be able to show users visually that
// their computation is currently active (as opposed to dead). To that end, we
// attach a message id to requests sent to the backend. All requests have a
// message id, but we only track evaluation requests here.

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';

import type ASIndex from '../classes/ASIndex';

type MessageMetadata = {
  messageId: string;
  messageTimestamp: number;
};

let _waitingIds: Map<ASIndex, MessageMetadata>  = new Map();

const ProgressStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register(action => {
    switch (action._type) {
      case Constants.ActionTypes.MARK_SENT: {
        const { index, messageId } = action;
        const metadata = {
          messageId,
          messageTimestamp: Date.now(),
        };
        _waitingIds.set(index, metadata);
        ProgressStore.emitChange();
        break;
      }

      case Constants.ActionTypes.MARK_RECEIVED: {
        const { index, messageId } = action;
        const metadata = _waitingIds.get(index);

        // The user might change the value of a cell before its evaluation has
        // been returned.
        //
        // 1 <---- request ----> 1
        //      2 <---- request ----> 2
        //
        // In which case we need to check that the message we received is the
        // one we're waiting on.
        if (metadata != null && metadata.messageId === messageId) {
          _waitingIds.delete(action.index);
          ProgressStore.emitChange();
        }
        break;
      }
    }
  }),

  getLocationsInProgress(): Map<ASIndex, MessageMetadata> {
    return _waitingIds;
  },
});

export default ProgressStore;
