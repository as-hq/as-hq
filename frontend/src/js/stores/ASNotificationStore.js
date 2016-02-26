/* @flow */

// Tracks notifications that are currently being displayed.

import shortid from 'shortid';

import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';

import NotificationRecord from '../classes/Notification';

// The data structure is a stack of notification IDs. The 'ADD_NOTIFICATION'
// action will push to the top of the stack, and any listening components
// will peek at the top to get the last added notification.
let _notifications: Array<string> = [];

const NotificationStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register(action => {
    switch (action._type) {
      case 'ADD_NOTIFICATION': {
        const notif = new NotificationRecord(action.spec);
        // $FlowFixMe
        _notifications.push(notif.uid);
        NotificationStore.emit('ADD', notif);
        break;
      }

      // This is for programmatic dismissal
      case 'DISMISS_NOTIFICATION': {
        const {uid} = action;
        NotificationStore._remove(uid);
        NotificationStore.emit('DISMISS', uid);
        break;
      }

      // This updates the internal state of the store in response to the user
      // dismissing a notification, and will not emit an event.
      case 'REMOVE_NOTIFICATION': {
        NotificationStore._remove(action.uid);
        break;
      }
    }
  }),

  peek(): string {
    return _notifications.slice(-1).pop();
  },

  _remove(uid: string) {
    const idx = _notifications.indexOf(uid);
    if (idx >= 0) {
      delete _notifications[idx];
    }
  }
});

export default NotificationStore;
