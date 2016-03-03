/* @flow */

import type { BottomPane } from '../types/State';

import Dispatcher from '../Dispatcher';
import API from './ASApiActionCreators';

export default {
  toggleHeader() {
    Dispatcher.dispatch({
      _type: 'HEADER_TOGGLED'
    });
  },

  // A slight misnomer. If the pane passed in is active, it toggles it; otherwise, it sets
  // the active pane to the passed in. 
  toggleBottomPane(pane: BottomPane) {
    Dispatcher.dispatch({
      _type: 'BOTTOM_PANE_TOGGLED',
      pane
    });
  },  

  setConnectedState(isConnected: boolean) {
    Dispatcher.dispatch({
      _type: 'SET_CONNECTING_STATE',
      isConnected
    });
  },

  repaintSpreadsheet() {
    Dispatcher.dispatch({
      _type: 'REPAINT_SPREADSHEET'
    });
  },

  submitBugReport() {
    const bugReport = window.prompt("Please describe the bug you encountered.","");
    API.bugReport(bugReport);
  },

  openFindBar() {
    Dispatcher.dispatch({
      _type: 'FIND_BAR_VISIBILITY_CHANGED',
      isOpen: true
    });
  },

  closeFindBar() {
    Dispatcher.dispatch({
      _type: 'FIND_BAR_VISIBILITY_CHANGED',
      isOpen: false
    })
  },

  openFindModal() {
    Dispatcher.dispatch({
      _type: 'FIND_MODAL_VISIBILITY_CHANGED',
      isOpen: true
    });
  },

  closeFindModal() {
    Dispatcher.dispatch({
      _type: 'FIND_MODAL_VISIBILITY_CHANGED',
      isOpen: false
    });
  },
}
