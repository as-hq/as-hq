/* @flow */

import React from 'react';
import App from './App.jsx';

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
