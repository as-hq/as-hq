// @flow
import Dispatcher from '../Dispatcher';

import type { FocusedElement } from '../types/State';

import ExpressionStore from '../stores/ASExpressionStore';

export default {
  // Set the focus to either the grid, textbox, or editor.
  focus(focus: FocusedElement) {
    Dispatcher.dispatch({
      _type: 'FOCUSED',
      focus,
    });
  },

  focusTextboxFully() {
    Dispatcher.dispatch({
      _type: 'FOCUSED_TEXTBOX_FULLY'
    });
  },

  toggleFocusF2() {
    // if not already editing, start.
    if (ExpressionStore.isEditing()) {
      Dispatcher.dispatch({
        _type: 'TOGGLED_FOCUS_F2',
      });
    // otherwise, standard toggling control flow.
    } else {
      Dispatcher.dispatch({
        _type: 'START_EDITING',
        textMutator: t => t,
        textboxHasFullFocus: false
      });
    }
  },
};
