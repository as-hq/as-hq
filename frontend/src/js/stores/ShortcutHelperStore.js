/* @flow */

import BaseStore from './BaseStore';
import Dispatcher from '../Dispatcher';

let _data = {
  shortcutHelperOpen: false,
};

let dispatcherIndex = Dispatcher.register((action) => {
  switch (action._type) {
    case 'CLEARED_SHEET':
      _data.shortcutHelperOpen = false;
      break;
    case 'TOGGLE_SHORTCUT_HELPER':
      _data.shortcutHelperOpen = !_data.shortcutHelperOpen;
      ShortcutHelperStore.emitChange();
      break;
    case 'CLOSE_SHORTCUT_HELPER':
      _data.shortcutHelperOpen = false
      ShortcutHelperStore.emitChange();
      break;
    default:
      break;
  }
});

const ShortcutHelperStore = Object.assign({}, BaseStore, {
  isShortcutHelperOpen(): boolean { return _data.shortcutHelperOpen; },
});

export default ShortcutHelperStore;
