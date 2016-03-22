/* @flow */

import type {
  ClientMessage,
  ServerMessage,
  UpdateSheet,
  MessageId,
} from '../types/Messages';

import ConversionU from '../AS/utils/Conversion';

import Dispatcher from '../Dispatcher';

export default {
  markSent(msg: any) {
    const {messageId, serverAction} = msg;
    const locations = ConversionU.getLocationsFromServerAction(serverAction);
    Dispatcher.dispatch({
      _type: 'MARK_SENT',
      locations,
      messageId
    });
  },

  markReceived(messageId: MessageId) {
    Dispatcher.dispatch({
      _type: 'MARK_RECEIVED',
      messageId,
    });
  },

  markAllReceived() {
    Dispatcher.dispatch({
      _type: 'MARK_ALL_RECEIVED'
    });
  }
}
