/* @flow */

import type {
  NotificationSpec
} from '../classes/Notification';

import shortid from 'shortid';
import Dispatcher from '../Dispatcher';

export default {
  addNotification(spec: NotificationSpec) {
    Dispatcher.dispatch({
      _type: 'ADD_NOTIFICATION',
      spec
    });
  },

  dismissNotification(uid: string) {
    Dispatcher.dispatch({
      _type: 'DISMISS_NOTIFICATION',
      uid
    });
  },

  dismissLast() {
    const uid = NotificationStore.peek();
    Dispatcher.dispatch({
      _type: 'DISMISS_NOTIFICATION',
      uid
    });
  },

  removeNotification(uid: string) {
    Dispatcher.dispatch({
      _type: 'REMOVE_NOTIFICATION',
      uid
    });
  }
}
