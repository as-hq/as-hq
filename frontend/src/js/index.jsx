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
import ModalStore from './stores/ASModalStore';
import LoginStore from './stores/ASLoginStore';
import LoginActions from './actions/ASLoginActionCreators';
import FocusActions from './actions/ASFocusActionCreators';
import GridActions from './actions/ASGridActionCreators';
import API from './actions/ASApiActionCreators';

// XXX(joel) - only include in dev
window.Perf = require('react-addons-perf');

window.imm = require('immutable');
window.exp = require('./actions/ASExpressionActionCreators');
window.grid = require('./actions/ASGridActionCreators');
window.idx = require('./classes/ASIndex');
window.focus = require('./actions/ASFocusActionCreators');
window.expstore = require('./stores/ASExpressionStore');
window.u = require('./AS/Util');
window.ex = require('./classes/ASExcelRef');
window.login = require('./stores/ASLoginStore');
window.conf = require('./stores/ASConfigurationStore');
window.gridstore = require('./stores/ASGridStore');
window.sheet = require('./stores/ASSheetStateStore');
window.cellstore = require('./stores/ASCellStore');
window.focusstore = require('./stores/ASFocusStore');
window.api = require('./actions/ASApiActionCreators');

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

function regularAuth(nextState, replace) {
  LoginActions.registerCallback(() => {
    API.openSheet();
  }, 'OpenSheet');
  // ^ register callback with an ID (in this case, 'OpenSheet')
  // so we don't register it more than once.
  //
  // `regularAuth` may be called many times due to routing. E.g.:
  // user goes to `/` =>
  // regularAuth() =>
  // redirect to `/login` =>
  // login success =>
  // redirect to `/app` =>
  // regularAuth()
  requireAuth(nextState, replace);
}

function referredSheetAuth(nextState, replace, isPublicReferral) {
  LoginActions.registerCallback(() => {
    const {referredSheetId} = nextState.params;
    API.acquireSheet(referredSheetId);
  }, 'AcquireSheet');
  // ^ see above comment for justification of callback ID
  if (isPublicReferral) {
    LoginActions.setPublicLogin();
  }
  requireAuth(nextState, replace);
}

const main = (
  <Router>
    <Route path="/" component={Index}>
      <IndexRoute component={Login} />
      <Route path="login"
             component={Login} />
      <Route path="app"
             component={App}
             onEnter={(ns, rep) => regularAuth(ns, rep)} />
      <Route path="sheets/:referredSheetId"
             component={App}
             onEnter={(ns, rep) => referredSheetAuth(ns, rep, false)} />
      <Route path="sheets/public/:referredSheetId"
             component={App}
             onEnter={(ns, rep) => referredSheetAuth(ns, rep, true)} />
    </Route>
  </Router>
);

// render the app when polymer is ready
document.addEventListener('polymer-ready', () => {
  // $FlowFixMe #flowlens
  render(main, document.getElementById('main'));
});

/*
When typing and evaluating rapidly in the grid, occasionally both the grid and
the editor lose focus. When this happens, restore focus.
 */
// $FlowFixMe ::ALEX::
document.addEventListener('keydown', (e) => {
  if (e.srcElement.tagName === 'BODY' && ! ModalStore.isAnyOpen()) {
    FocusActions.focus('grid');
    GridActions.executeKey(e);
  }
});
