/* @flow */

import type {
  NotificationSpec
} from '../types/Notifications';

import type {
  Callback
} from '../types/Base';

import React from 'react';

// $FlowFixMe
import NotificationSystem from 'react-notification-system';

import NotificationStore from '../stores/ASNotificationStore';
import * as NotificationActions from '../actions/ASNotificationActionCreators';

export default class NotificationController extends React.Component<{}, {}, {}> {
  _notificationSystem: any;
  _addNotificationListener: (uid: string, spec: NotificationSpec) => void;
  _dismissNotificationListener: Callback<string>;

  constructor(props: {}) {
    super(props);
    this._addNotificationListener = (uid, spec) =>
      this._addNotification(uid, spec);
    this._dismissNotificationListener = (uid) =>
      this._dismissNotification(uid);
  }

  componentDidMount() {
    NotificationStore.on('ADD', this._addNotificationListener);
    NotificationStore.on('DISMISS', this._dismissNotificationListener);
  }

  componentWillUnmount() {
    NotificationStore.removeListener('ADD', this._addNotificationListener);
    NotificationStore.removeListener('DISMISS', this._dismissNotificationListener);
  }

  render(): React.Element {
    return <NotificationSystem ref={elem => this._notificationSystem = elem} />
  }

  _addNotification(uid: string, spec: NotificationSpec) {
    this._notificationSystem.addNotification({
      uid,
      position: 'br',
      autoDismiss: false,
      dismissible: true,
      onRemove: (notification) => {
        NotificationActions.removeNotification(notification.uid);
      },
      ...spec
    });
  }

  _dismissNotification(uid: string) {
    this._notificationSystem.removeNotification(uid);
  }
}
