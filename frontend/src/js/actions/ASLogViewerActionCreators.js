/* @flow */

import Dispatcher from '../Dispatcher';

export function openLogViewer() {
  Dispatcher.dispatch({
    _type: 'LOG_VIEWER_OPENED'
  });
}