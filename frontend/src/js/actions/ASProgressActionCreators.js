/* @flow */

import type {
  ClientMessage,
  ServerMessage,
  UpdateSheet,
} from '../types/Messages';

import Dispatcher from '../Dispatcher';

export function markSent(msg: ServerMessage) {
  const {messageId, serverAction} = msg;
  if (serverAction.tag === 'Evaluate') {
    Dispatcher.dispatch({
      _type: 'MARK_SENT',
      index: serverAction.contents[0].evalLoc.index,
      messageId,
    });
  }
}

export function markReceived(msg: ClientMessage) {
  const {messageId, clientAction} = msg;
  if (clientAction.tag === 'UpdateSheet') {
    const locations = (clientAction: UpdateSheet).contents.cellUpdates.newVals;

    const indices = locations.map(({cellLocation: {index}}) => index);
    for (const index of indices) {
      Dispatcher.dispatch({
        _type: 'MARK_RECEIVED',
        index,
        messageId,
      });
    }
  }
}

export function clearProgress(messageId: string) {
  Dispatcher.dispatch({
    _type: 'CLEAR_PROGRESS',
    messageId
  });
}

export function clearAllProgress() {
  Dispatcher.dispatch({
    _type: 'CLEAR_ALL_PROGRESS'
  });
}
