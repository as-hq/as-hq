// @flow
import Dispatcher from '../Dispatcher';

import type {ASFocusType, FocusStoreCallbacks} from '../types/State';

export default {
  // Set the focus to either the grid, textbox, or editor.
  setFocus(focus: ASFocusType) {
    Dispatcher.dispatch({
      _type: 'FOCUSED',
      focus,
    });
  },

  // Initialize the callbacks that the focus manager uses to refocus on the
  // editor, grid, or textbox. This should be called once.
  setCallbacks(callbacks: FocusStoreCallbacks) {
    Dispatcher.dispatch({
      _type: 'SET_FOCUS_CALLBACKS',
      callbacks,
    });
  },

  // TODO(joel) - make shortcut action creators:
  //
  // Focus store should just listen for the action indicating F2 was pressed.
  // All shortcuts create an action, shortcuts.js looks radically different.
  toggleFocusF2() {
    Dispatcher.dispatch({
      _type: 'TOGGLED_FOCUS_F2',
    });
  },
};
