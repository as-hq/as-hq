// @flow

import Dispatcher from '../Dispatcher';

export default {
  openCondFormattingDialog() {
    Dispatcher.dispatch({
      _type: 'OPEN_COND_FORMATTING_DIALOG',
    });
  },

  closeCondFormattingDialog() {
    Dispatcher.dispatch({
      _type: 'CLOSE_COND_FORMATTING_DIALOG',
    });
  },

  openChartingDialog() {
    Dispatcher.dispatch({
      _type: 'OPEN_CHARTING_DIALOG',
    });
  },

  closeChartingDialog() {
    Dispatcher.dispatch({
      _type: 'CLOSE_CHARTING_DIALOG',
    });
  }
}
