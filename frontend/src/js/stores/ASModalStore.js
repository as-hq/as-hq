/* @flow */

import BaseStore from './BaseStore';
import Dispatcher from '../Dispatcher';

let _data = {
  condFormattingOpen: false,
  chartOpen: false,
  shareOpen: false,
};

let dispatcherIndex = Dispatcher.register((action) => {
  switch (action._type) {
    case 'CLEARED_SHEET':
      _data = {
        condFormattingOpen: false,
        chartOpen: false,
        shareOpen: false,
      };
      break;
    case 'CLOSE_COND_FORMATTING_DIALOG':
      _data.condFormattingOpen = false;
      ASModalStore.emitChange();
      break;
    case 'OPEN_COND_FORMATTING_DIALOG':
      _data.condFormattingOpen = true;
      ASModalStore.emitChange();
      break;
    case 'CLOSE_CHARTING_DIALOG':
      _data.chartOpen = false;
      ASModalStore.emitChange();
      break;
    case 'OPEN_CHARTING_DIALOG':
      _data.chartOpen = true;
      ASModalStore.emitChange();
      break;
    case 'OPEN_SHARE_DIALOG':
      _data.shareOpen = true;
      ASModalStore.emitChange();
      break;
    case 'CLOSE_SHARE_DIALOG':
      _data.shareOpen = false;
      ASModalStore.emitChange();
      break;
  }
});

const ASModalStore = Object.assign({}, BaseStore, {
  getCondFormattingOpen(): boolean { return _data.condFormattingOpen; },
  getChartingOpen(): boolean { return _data.chartOpen; },
  getShareOpen(): boolean { return _data.shareOpen; },
  isAnyOpen(): boolean {
    return (
      _data.condFormattingOpen ||
      _data.chartOpen ||
      _data.shareOpen
    );
  },
});

export default ASModalStore;
