/* @flow */

import BaseStore from './BaseStore';
import Dispatcher from '../Dispatcher';

let _data = {
  condFormattingOpen: false,
  chartOpen: false
};

let dispatcherIndex = Dispatcher.register((action) => {
  switch (action._type) {
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
  }
});

const ASModalStore = Object.assign({}, BaseStore, {
  getCondFormattingOpen(): boolean { return _data.condFormattingOpen; },
  getChartingOpen(): boolean { return _data.chartOpen; }
});

export default ASModalStore;
