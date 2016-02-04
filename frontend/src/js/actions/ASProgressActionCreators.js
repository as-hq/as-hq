/* @flow */

import type {
  ClientMessage,
  ServerMessage,
  UpdateSheet,
  MessageId,
} from '../types/Messages';

import ConversionU from '../AS/utils/Conversion';

import Dispatcher from '../Dispatcher';

export function markSent(msg: any) { // should be ServerMessage but flow is fucking up
  const {messageId, serverAction} = msg;
  const locations = ConversionU.getLocationsFromServerAction(serverAction);
  if (locations.length > 0) {
    Dispatcher.dispatch({
      _type: 'MARK_SENT',
      locations,
      messageId
    });
  }
}

export function markReceived(messageId: MessageId) {
  Dispatcher.dispatch({
    _type: 'MARK_RECEIVED',
    messageId,
  });
}

export function markAllReceived() {
  Dispatcher.dispatch({
    _type: 'MARK_ALL_RECEIVED'
  });
}
