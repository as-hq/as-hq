/* @flow */

import type {ASLanguage} from '../types/Eval';
import type {EditorSelection} from '../types/Editor';

import Dispatcher from '../Dispatcher';
import ExpressionStore from '../stores/ASExpressionStore';
import FocusStore from '../stores/ASFocusStore';
import APIActions from '../actions/APIActionCreators';

import GridActions from '../actions/ASGridActionCreators';

import U from '../AS/Util';
import { actions as Shortcuts } from '../AS/Shortcuts';

const ExpressionActions = {
  resync() {
    Dispatcher.dispatch({
      _type: 'EXPRESSION_STORE_RESYNC'
    });
  },

  setLanguage(language: ASLanguage) {
    Dispatcher.dispatch({
      _type: 'LANGUAGE_CHANGED',
      language
    });

    if (!ExpressionStore.isEditing() && ExpressionStore.getExpression() != "") {
      APIActions.evaluate({dX: 0, dY: 0});
    }
  },

  setSelection(selection: EditorSelection) {
    Dispatcher.dispatch({
      _type: 'EDITOR_SELECTION_CHANGED',
      selection,
    });
  },

  setExpression(expression: string, eventSource: EditorSelectionEventSource) {
    Dispatcher.dispatch({
      _type: 'EXPRESSION_CHANGED',
      expression,
      eventSource
    });
  },

  startEditing(textMutator: (t: string) => string, textboxHasFullFocus: boolean) {
    Dispatcher.dispatch({
      _type: 'START_EDITING',
      textMutator,
      textboxHasFullFocus
    });
  },

  /**
   * Same as 'startEditing', but buffered; that is, every keystroke
   * after ExpressionStore.isEditing() is true will _add_ to the expression,
   * not replace it.
   * This is because 'fast' typers will execute multiple keystrokes between
   * the START_EDITING action and the textbox actually taking focus.
   */
  startEditingBuffered(inputText: string) {
    if (! ExpressionStore.isEditing()) {
      ExpressionActions.startEditing(_ => inputText, false);
    } else {
      ExpressionActions.startEditing(text => text + inputText, false);
    }
  },

  stopEditing() {
    Dispatcher.dispatch({
      _type: 'STOP_EDITING'
    });
  },

  toggleReference() {
    Dispatcher.dispatch({
      _type: 'REFERENCE_TOGGLED'
    });
  },

  executeTextboxKey(e: SyntheticKeyboardEvent) {
    const hasFullFocus = FocusStore.textboxHasFullFocus();

    if (U.Key.isNavKey(e) && (
      ! hasFullFocus ||
      ExpressionStore.isInsertingRef()
    ))  {
      GridActions.executeGridOnlyKey(e);
    } else {
      Shortcuts.try(e, 'textbox');
    }
  }
}

export default ExpressionActions;
