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
  },

  openShareDialog() {
    Dispatcher.dispatch({
      _type: 'OPEN_SHARE_DIALOG'
    });
  },

  closeShareDialog() {
    Dispatcher.dispatch({
      _type: 'CLOSE_SHARE_DIALOG'
    });
  },

  openAutoEvalDialog() {
    Dispatcher.dispatch({
      _type: 'OPEN_AUTO_EVAL_DIALOG'
    });
  },

  closeAutoEvalDialog() {
    Dispatcher.dispatch({
      _type: 'CLOSE_AUTO_EVAL_DIALOG'
    });
  },
}
