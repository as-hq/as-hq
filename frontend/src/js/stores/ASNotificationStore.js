/* @flow */

// Tracks notifications that are currently being displayed.

import shortid from 'shortid';

import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';
import LoginStore from './ASLoginStore';

import NotificationRecord from '../classes/Notification';

// The data structure is a stack of notification IDs. The 'ADD_NOTIFICATION'
// action will push to the top of the stack, and any listening components
// will peek at the top to get the last added notification.
let _notifications: Array<string> = [];

const NotificationStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register(action => {
    switch (action._type) {
      case 'CLEARED_SHEET':
        _notifications = [];
        break;

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

      case 'GOT_FAILURE': {
        const notif = new NotificationRecord({
          title: 'Error',
          message: action.errorMsg,
          level: 'error',
          autoDismiss: 2
        });
        // $FlowFixMe #flowlens
        _notifications.push(notif.uid);
        NotificationStore.emit('ADD', notif);
        break;
      }

      case 'LOGIN_SUCCESS': {
        _notifications.forEach(uid => {
          NotificationStore.emit('DISMISS', uid);
        });
        _notifications = [];
        break;
      }

      case 'LOGIN_FAILURE': {
        const uid = _notifications.pop();
        if (uid != undefined) {
          NotificationStore.emit('DISMISS', uid);
        }
        const notif = new NotificationRecord({
          title: 'Login failure: ' + action.failureReason,
          message: 'trying again...',
          level: 'error',
          position: 'bc'
        });
        // $FlowFixMe #flowlens
        _notifications.push(notif.uid);
        NotificationStore.emit('ADD', notif);
        break;
      }

      case 'SET_CONNECTING_STATE': {
        if (action.isConnected) {
          _notifications.forEach(uid => {
            NotificationStore.emit('DISMISS', uid);
          });
          _notifications = [];
        } else if (! LoginStore.isLoggedIn()){
          const notif = new NotificationRecord({
            title: 'Cannot reach server.',
            message: 'Are you connected to the internet?',
            level: 'error',
            position: 'bc'
          });
          // $FlowFixMe #flowlens
          _notifications.push(notif.uid);
          NotificationStore.emit('ADD', notif);
        }
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
