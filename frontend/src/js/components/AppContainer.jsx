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

import NotificationController from './NotificationController.jsx';

// $FlowFixMe
import injectTapEventPlugin from 'react-tap-event-plugin';
injectTapEventPlugin();

export default class AppContainer extends React.Component<{}, {}, {}> {

  render(): React.Element {
    return (
      <div style={{height: '100%', width: '100%'}}>
        <App />
        <NotificationController />
      </div>
    );
  }

};
