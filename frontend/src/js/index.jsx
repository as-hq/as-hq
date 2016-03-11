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
import FocusActions from './actions/ASFocusActionCreators';
import GridActions from './actions/ASGridActionCreators';

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

/*
When typing and evaluating rapidly in the grid, occasionally both the grid and
the editor lose focus. When this happens, restore focus.
 */
document.addEventListener('keydown', (e) => {
  if (e.srcElement.tagName === 'BODY') {
    FocusActions.focus('grid');
    GridActions.executeKey(e);
  }
});
