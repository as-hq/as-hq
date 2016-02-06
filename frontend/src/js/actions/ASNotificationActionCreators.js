/* @flow */

import type {
  NotificationSpec
} from '../types/Notifications';

import shortid from 'shortid';
import Dispatcher from '../Dispatcher';

export function addNotification(spec: NotificationSpec) {
  Dispatcher.dispatch({
    _type: 'ADD_NOTIFICATION',
    spec
  });
}

export function addSimpleNotification(title: string) {
  Dispatcher.dispatch({
    _type: 'ADD_NOTIFICATION',
    spec: {
      uid: shortid.generate(),
      title,
      autoDismiss: 1, // auto-dismiss after 1 sec
      level: 'success'
    }
  })
}

export function dismissNotification(uid: string) {
  Dispatcher.dispatch({
    _type: 'DISMISS_NOTIFICATION',
    uid
  });
}

export function removeNotification(uid: string) {
  Dispatcher.dispatch({
    _type: 'REMOVE_NOTIFICATION',
    uid
  });
}
