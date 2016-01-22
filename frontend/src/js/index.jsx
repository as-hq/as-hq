/* @flow */

import React from 'react';
import {render} from 'react-dom';
import App from './components/App.jsx';
import AppContainer from './components/AppContainer.jsx';

// XXX(joel) - only include in dev
window.Perf = require('react-addons-perf');

render(<AppContainer />, document.getElementById('main'));
