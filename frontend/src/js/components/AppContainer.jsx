/* @flow */

import type {
  NotificationSpec
} from '../types/Notifications';

import type {
  Callback
} from '../types/Base';

import React from 'react';
import App from './App.jsx';
// $FlowFixMe
import NotificationSystem from 'react-notification-system';

import NotificationStore from '../stores/ASNotificationStore';
import * as NotificationActions from '../actions/ASNotificationActionCreators';

// $FlowFixMe
import injectTapEventPlugin from 'react-tap-event-plugin';
injectTapEventPlugin();


export default class AppContainer extends React.Component<{}, {}, {}> {

  _notificationSystem: any;
  _addNotificationListener: any;
  _dismissNotificationListener: any;

  constructor(props: {}) {
    super(props);
    this._addNotificationListener = (uid, spec) =>
      this._addNotification(uid, spec);
    this._dismissNotificationListener = (uid) =>
      this._dismissNotification(uid);
  }

  componentDidMount() {
    this._notificationSystem = this.refs.notificationSystem;
    NotificationStore.on('ADD', this._addNotificationListener);
    NotificationStore.on('DISMISS', this._dismissNotificationListener);
  }

  componentWillUnmount() {
    NotificationStore.removeListener('ADD', this._addNotificationListener);
    NotificationStore.removeListener('DISMISS', this._dismissNotificationListener);
  }

  // Note: the notification system is in this top-level component, at the
  // author's suggestion that doing so would avoid positioning issues.
  // https://github.com/igorprado/react-notification-system
  render(): React.Element {
    return (
      <div style={{height: '100%', width: '100%'}}>
        <App />
        <NotificationSystem ref="notificationSystem" />
      </div>
    );
  }

  _addNotification(uid: string, spec: NotificationSpec) {
    this._notificationSystem.addNotification({
      uid,
      position: 'br',
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
};
