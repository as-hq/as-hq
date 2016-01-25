/* @flow */

import type {
  NotificationSpec
} from '../types/Notifications';

import Dispatcher from '../Dispatcher';

export function addNotification(spec: NotificationSpec) {
  Dispatcher.dispatch({
    _type: 'ADD_NOTIFICATION',
    spec
  });
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
