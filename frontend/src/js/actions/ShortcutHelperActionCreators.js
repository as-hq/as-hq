// @flow

import Dispatcher from '../Dispatcher';

export default {

  toggleShortcutHelper() {
    Dispatcher.dispatch({
      _type: 'TOGGLE_SHORTCUT_HELPER',
    });
  },

  closeShortcutHelper() {
    Dispatcher.dispatch({
      _type: 'CLOSE_SHORTCUT_HELPER',
    });
  }
  
}
