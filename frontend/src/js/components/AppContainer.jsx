/* @flow */

import type {
  NotificationSpec
} from '../types/Notifications';


import React from 'react';
import ReactU from '../AS/utils/React';
import App from './App.jsx';
// $FlowFixMe
import NotificationSystem from 'react-notification-system';

import NotificationStore from '../stores/ASNotificationStore';
import {removeNotification} from '../actions/ASNotificationActionCreators';

// $FlowFixMe
import injectTapEventPlugin from 'react-tap-event-plugin';
injectTapEventPlugin();


export default class AppContainer extends React.Component<{}, {}, {}> {

  _notificationSystem: any;

  constructor(props: {}) {
    super(props);
  }

  componentDidMount() {
    this._notificationSystem = this.refs.notificationSystem;
    NotificationStore.on('ADD', this._addNotification);
    NotificationStore.on('DISMISS', this._dismissNotification);
  }

  componentWillUnmount() {
    NotificationStore.removeListener('ADD', this._addNotification);
    NotificationStore.removeListener('DISMISS', this._dismissNotification);
  }

  // Note: the notification system is in this top-level component, at the
  // author's suggestion that doing so would avoid positioning issues.
  // https://github.com/igorprado/react-notification-system
  render(): React.Element {
    return (
      <div>
        <App />
        <NotificationSystem ref="notificationSystem" />
      </div>
    );
  }

  _addNotification(uid: string, spec: NotificationSpec) {
    this._notificationSystem.addNotification({
      uid,
      position: 'br',
      autoDismiss: false,
      dismissible: true,
      onRemove: (notification) => {
        removeNotification(notification.uid);
      },
      ...spec
    });
  }

  _dismissNotification(uid: string) {
    this._notificationSystem.removeNotification(uid);
  }
};
