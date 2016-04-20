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
import FocusStore from './stores/ASFocusStore';
import LoginActions from './actions/ASLoginActionCreators';
import FocusActions from './actions/ASFocusActionCreators';
import GridActions from './actions/ASGridActionCreators';
import API from './actions/ASApiActionCreators';
import APIActions from './actions/APIActionCreators';

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
window.loginactions = require('./actions/ASLoginActionCreators');
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

function regularAuth(nextState, replace) {
  if (! LoginStore.isLoggedIn()) {
    LoginActions.registerCallback(() => {
      console.error('REGULAR AUTH!!');
      APIActions.openSheet();
    });
    replace({ nextPathName: nextState.location.pathname }, '/login');
  }
}

function referredSheetAuth(nextState, replace, isPublicReferral) {
  if (! LoginStore.isLoggedIn()) {
    LoginActions.registerCallback(() => {
      const {referredSheetId} = nextState.params;
      console.error('REFERRED AUTH!!!');
      APIActions.acquireSheet(referredSheetId);
    });
    if (isPublicReferral) {
      LoginActions.setPublicLogin();
    }
    replace({ nextPathName: nextState.location.pathname }, '/login');
  }
}

const main = (
  <Router>
    <Route path="/" component={Index}>
      <IndexRoute component={App}
                  onEnter={(ns, rep) => regularAuth(ns, rep)} />
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
  if (e.srcElement.tagName === 'BODY' && FocusStore.getFocus() === 'grid') {
    // resync grid focus
    FocusActions.focus('grid');
    GridActions.executeKey(e);
  }
});
