/* @flow */

import type {
  ClientMessage,
  ServerMessage,
  UpdateSheet,
} from '../types/Messages';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

export function markSent(msg: ServerMessage) {
  const {messageId, serverAction} = msg;
  if (serverAction.tag === 'Evaluate') {
    Dispatcher.dispatch({
      _type: Constants.ActionTypes.MARK_SENT,
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
        _type: Constants.ActionTypes.MARK_RECEIVED,
        index,
        messageId,
      });
    }
  }
}
