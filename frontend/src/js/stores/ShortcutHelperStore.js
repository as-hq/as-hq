/* @flow */

import BaseStore from './BaseStore';
import Dispatcher from '../Dispatcher';

let _data = {
  isOpen: false,
};

let dispatcherIndex = Dispatcher.register((action) => {
  switch (action._type) {
    case 'TOGGLE_SHORTCUT_HELPER':
      _data.isOpen = !_data.isOpen;
      ShortcutHelperStore.emitChange();
      break;
    case 'CLOSE_SHORTCUT_HELPER':
      _data.isOpen = false;
      ShortcutHelperStore.emitChange();
      break;
    default:
      break;
  }
});

const ShortcutHelperStore = Object.assign({}, BaseStore, {
  isOpen(): boolean { return _data.isOpen; },
});

export default ShortcutHelperStore;
