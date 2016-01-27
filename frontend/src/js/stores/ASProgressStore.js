/* @flow */

// Track progress in cells. We want to be able to show users visually that
// their computation is currently active (as opposed to dead). To that end, we
// attach a message id to requests sent to the backend. All requests have a
// message id, but we only track evaluation requests here.

import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';

import type {
  NakedIndex
} from '../types/Eval';

type MessageMetadata = {
  messageId: string;
  messageTimestamp: number;
};

type ProgressStoreData = Map<number, Map<number, MessageMetadata>>;

let _waitingIds: ProgressStoreData = new Map();

const ProgressStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register(action => {
    switch (action._type) {
      case 'MARK_SENT': {
        const { index, messageId } = action;
        const metadata = {
          messageId,
          messageTimestamp: Date.now(),
        };

        let colSet = _waitingIds.get(index.col);
        if (colSet !== undefined) {
          colSet.set(index.row, metadata); // will modify _waitingIds. would be preferable to use ImmutableJS here
        } else {
          _waitingIds.set(
            index.col,
            new Map().set(index.row, metadata)
          );
        }
        ProgressStore.emitChange();
        break;
      }

      case 'MARK_RECEIVED': {
        const { index, messageId } = action;

        let colSet = _waitingIds.get(index.col);
        if (colSet !== undefined) {
          const metadata = colSet.get(index.row);

          // The user might change the value of a cell before its evaluation has
          // been returned.
          //
          // 1 <---- request ----> 1
          //      2 <---- request ----> 2
          //                            ^ mark finished here
          //
          // In which case we need to check that the message we received is the
          // one we're waiting on.
          if (metadata != undefined && metadata.messageId === messageId) {
            colSet.delete(index.row);
            ProgressStore.emitChange();
          }
        }
        break;
      }

      case 'CLEAR_PROGRESS': {
        const loc = ProgressStore.lookup(action.messageId);
        if (!! loc) {
          let colSet = _waitingIds.get(loc.col);
          if (!! colSet) {
            colSet.delete(loc.row);
          }
          ProgressStore.emitChange();
        }
      }

      case 'CLEAR_ALL_PROGRESS': {
        _waitingIds = new Map();
        ProgressStore.emitChange();
      }
    }
  }),

  getLocationsInProgress(): ProgressStoreData {
    return _waitingIds;
  },

  lookup(myMessageId: string): ?NakedIndex {
    for (const [col, colSet] of _waitingIds) {
      for (const [row, {messageId}] of colSet) {
        if  (messageId === myMessageId) {
          return {col, row};
        }
      }
    }
  }

});

export default ProgressStore;
