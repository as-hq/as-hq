/* @flow */

import type {
  Callback
} from '../types/Base';

import type {
  Notification
} from '../classes/Notification';

import React from 'react';

// $FlowFixMe
import NotificationSystem from 'react-notification-system';

import NotificationStore from '../stores/ASNotificationStore';
import NotificationActions from '../actions/ASNotificationActionCreators';

export default class NotificationController extends React.Component {
  static defaultProps = {}; 
  props: {};
  state: {};

  _notificationSystem: any;
  _addNotificationListener: Callback<Notification>;
  _dismissNotificationListener: Callback<string>;

  constructor(props: {}) {
    super(props);
    this._addNotificationListener = (notif) =>
      this._addNotification(notif);
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

  _addNotification(notif: Notification) {
    this._notificationSystem.addNotification({
      onRemove: (thisNotif) => {
        NotificationActions.removeNotification(thisNotif.uid);
      },
      ...notif.toJS()
    });
  }

  _dismissNotification(uid: string) {
    this._notificationSystem.removeNotification(uid);
  }
}
