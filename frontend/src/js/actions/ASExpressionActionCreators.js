/* @flow */

import type {ASLanguage} from '../types/Eval';
import type {EditorSelection} from '../types/Editor';

import Dispatcher from '../Dispatcher';
import ExpressionStore from '../stores/ASExpressionStore';
import FocusStore from '../stores/ASFocusStore';
import GridStore from '../stores/ASGridStore';
import APIActions from '../actions/APIActionCreators'; 
import API from '../actions/ASApiActionCreators'; 
// API and APIActions...? pretty terrible... (Alex 3/11/2016)

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
      API.setLanguagesInRange(language, GridStore.getActiveSelection().range);
    }
  },

  setSelection(selection: EditorSelection) {
    Dispatcher.dispatch({
      _type: 'EDITOR_SELECTION_CHANGED',
      selection,
    });
  },

  setExpression(expression: string) {
    Dispatcher.dispatch({
      _type: 'EXPRESSION_CHANGED',
      expression
    });
  },

  startEditing(initialText: string, textboxHasFullFocus: boolean) {
    Dispatcher.dispatch({
      _type: 'START_EDITING',
      initialText,
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
      ExpressionActions.startEditing(inputText, false);
    } else {
      const text = ExpressionStore.getExpression();
      ExpressionActions.startEditing(text + inputText, false);
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
