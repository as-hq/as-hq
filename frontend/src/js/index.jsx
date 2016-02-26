/* @flow */

import type {
  RoutedComponentProps
} from './types/Router';

import { render } from 'react-dom';
import React from 'react';
// $FlowFixMe
import { Router, Route, IndexRoute } from 'react-router';

import App from './components/App.jsx';
import Login from './components/Login.jsx';
import NotificationController from './components/NotificationController.jsx';
import LoginStore from './stores/ASLoginStore';

// XXX(joel) - only include in dev
window.Perf = require('react-addons-perf');

import API from './actions/ASApiActionCreators';
window.API = API;
window.cells = require('./stores/ASCellStore');
window.idx = require('./classes/ASIndex');

const Index = ({children}) => (
  <div className="full">
    {React.Children.only(children)}
    <NotificationController />
  </div>
);

function requireAuth(nextState, replace) {
  if (! LoginStore.isLoggedIn()) {
    replace({ nextPathName: nextState.location.pathname }, '/login');
  }
}

const main = (
  <Router>
    <Route path="/" component={Index}>
      <IndexRoute component={Login} />
      <Route path="login" component={Login} />
      <Route path="app" component={App} onEnter={requireAuth} />
    </Route>
  </Router>
);

// render the app when polymer is ready
document.addEventListener('polymer-ready', () => {
  render(main, document.getElementById('main'));
});
