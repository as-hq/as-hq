/* @flow */

import type {
  ASAction
} from '../types/Actions';

import type {
  ASLanguage,
  NakedIndex
} from '../types/Eval';

import type {
  FocusedElement
} from '../types/State';

import type { EditorSelection } from '../types/Editor';

import ASIndex from '../classes/ASIndex';
import ASSelection from '../classes/ASSelection';

import dispatcher from '../Dispatcher';
import Immutable from 'immutable';
// $FlowFixMe
import {ReduceStore} from 'flux/utils';
import API from '../actions/ASApiActionCreators';
import CellStore from '../stores/ASCellStore';
import GridStore from '../stores/ASGridStore';
import FocusStore from '../stores/ASFocusStore';

import Render from '../AS/Renderers';
import U from '../AS/Util';

// #flowlens
type State = any;

const StateRecord = Immutable.Record({
  expression: '',
  selection: {
    range: {
      start: {row: 0, column: 0},
      end: {row: 0, column: 0}
    },
    backwards: false
  },
  currentLanguage: 'Excel',
  defaultLanguage: 'Excel',
  isEditing: false,
  isInsertingRef: false,
  textboxPosition: null
});

class ExpressionStore extends ReduceStore<State> {

  getInitialState(): State {
    return new StateRecord();
  }

  // #flowaction
  reduce(state: State, action: any) {
    switch(action._type) {
      case 'EXPRESSION_STORE_RESYNC': {
        this.__emitChange();
        return state;
      }

      case 'LOGIN_SUCCESS': {
        const sheetId = action.openedWorkbook.openedSheet;
        return state.set('textboxPosition', ASIndex.fromNaked({col: 1, row: 1}, sheetId));
      }

      case 'FOCUSED': {
        const {focus} = action;
        let state_ = state;

        if (focus === 'editor' && !state_.isEditing) {
          state_ = state_.set('isEditing', true);
        }
        if (focus === 'editor' || focus === 'textbox') {
          state_ = state_.set('isInsertingRef', false);
        }
        return state_;
      }

      case 'FOCUSED_TEXTBOX_FULLY': {
        return state.set('isInsertingRef', false);
      }

      case 'EXPRESSION_CHANGED': {
        const {expression} = action;

        if (expression === state.expression) {
          // sometimes, the expression actually didn't change
          // this happens when you press the cmd key on macs
          // in those cases, we don't want to do isInsertingRef: false
          // basically, Textbox for some reason fires requestChange
          // so then there is an action fired as well.

          return state;
        } else {
          const {currentLanguage} = state;

          highlightAncestors(expression, currentLanguage);

          return state.merge({
            expression,
            isInsertingRef: false
          });
        }
      }

      case 'LANGUAGE_CHANGED': {
        const {language} = action;
        return state.merge({
          currentLanguage: language,
          defaultLanguage: language
        });
      }

      case 'EDITOR_SELECTION_CHANGED': {
        return state.set('selection', action.selection);
      }

      case 'API_EVALUATE': {
        this.getDispatcher().waitFor([CellStore.getDispatchToken(), GridStore.getDispatchToken()]);

        const {origin} = GridStore.getActiveSelection();
        return displayActiveExpression(state, origin);
      }

      case 'SELECTION_CHANGED': {
        this.getDispatcher().waitFor([GridStore.getDispatchToken()]);

        const { selection } = action;
        if (state.isEditing) {
          return tryInsertingRef(state, selection);
        } else {
          return displayActiveExpression(state, selection.origin);
        }
      }

      case 'START_EDITING': {
        const {initialText} = action;
        const initialSelection = U.String.getInitialSelectionForText(initialText);

        // Merge` is deep, so `selection` being an naked object and not a class,
        // will get turned into an immutable Map, necessitating a bunch of ugly
        // `toJS()`/`fromJS()` conversions. The correct thing to do here is make
        // `EditorSelection` a class.
        return state.merge({
          isEditing: true,
          expression: initialText
        }).set('selection', initialSelection);
      }

      case 'REFERENCE_TOGGLED': {
        if (state.isEditing) {
          return tryTogglingRef(state);
        }
        return state;
      }

      case 'STOP_EDITING': {
        // revert to expression originally present in cell
        const { origin } = GridStore.getActiveSelection();
        return displayActiveExpression(state, origin);
      }

      case 'FIND_BAR_VISIBILITY_CHANGED': {
        return state.set('isEditing', false);
      }

      case 'SHEET_UPDATED': {
        this.getDispatcher().waitFor([CellStore.getDispatchToken()]);

        // If I am editing, the active expression being displayed is
        // the one I have just typed, in which case we shouldn't update it.
        if (! state.isEditing) {
          const {origin} = GridStore.getActiveSelection();
          return displayActiveExpression(state, origin);
        }
        return state;
      }

      default:
        return state;
    }
  }

  getExpression(): string {
    return this.getState().expression;
  }

  getLanguage(): ASLanguage {
    return this.getState().currentLanguage;
  }

  getDefaultLanguage(): ASLanguage {
    return this.getState().defaultLanguage;
  }

  getSelection(): EditorSelection {
    return this.getState().selection;
  }

  getTextboxPosition(): ASIndex {
    return this.getState().textboxPosition;
  }

  isEditing(): boolean {
    return this.getState().isEditing;
  }

  isInsertingRef(): boolean {
    return this.getState().isInsertingRef;
  }

  canInsertRefFromEditor(editor: FocusedElement): boolean {
    const {expression, selection, isInsertingRef} = this.getState();
    if (isInsertingRef)
      return true;

    const shouldTryInserting = (
      (editor === 'textbox' && !FocusStore.textboxHasFullFocus())
    );

    return (
      shouldTryInserting &&
      U.String.selectionIsEnd(selection, expression) &&
      U.Parsing.canInsertRef(expression, '')
    );
  }
}

function highlightAncestors(exp: string, lang: ?ASLanguage) {
  const deps = U.Parsing.parseDependencies(exp, lang);
  Render.setDependencies(deps);
}


/**
 * This returns all editing functionality to the "ground" state;
 * that is:
 * (1) displaying the untainted expression at activeSelection.origin,
 * (2) not inserting refs.
 * (3) not currently editing.
 * (4) textboxPosition === activeSelection.origin
 *
 * Pass in the origin as an argument, because the active "selection" according
 * to GridStore is not always the displayed expression (e.g. during ref insertion)
 */
function displayActiveExpression(state: State, origin: ASIndex): State {
  const cell = CellStore.getCell(origin);
  const expression = (!! cell) ? cell.expression.expression : '';
  const currentLanguage = (!! cell) ? cell.expression.language : state.defaultLanguage;

  highlightAncestors(expression, currentLanguage);

  return state.merge({
    expression,
    currentLanguage,
    isInsertingRef: false,
    isEditing: false,
    textboxPosition: origin
  });
}


/**
 * Attempts to insert a ref.
 * Must wait on GridStore before calling.
 */
function tryInsertingRef(state: State, gridSelection: ASSelection): State {
  const {selection, expression, currentLanguage, isInsertingRef, textboxPosition} = state;
  const [prefix, suffix] = U.String.splitOnSelection(expression, selection);
  const ref = gridSelection.toExcelString();
  const {row, column} = U.String.getSelectionLead(selection);

  if (isInsertingRef) {

    // trim old ref, insert the new ref, move selection in front.
    const oldRef = GridStore.getLastActiveSelection().toExcelString();
    const newExpression = prefix.slice(0, -1*oldRef.length) + ref + suffix;
    const newLead = {row, column: column - oldRef.length + ref.length};
    const newSelection = {
      range: {start: newLead, end: newLead},
      backwards: false
    };

    return state.set('expression', newExpression)
                .set('selection', newSelection);

  } else if (U.Parsing.canInsertRef(prefix, suffix)) {

    // insert the ref, and move the selection in front of it.
    const newExpression = prefix + ref + suffix;
    const newLead = {row, column: column + ref.length};
    const newSelection = {
      range: {start: newLead, end: newLead},
      backwards: false
    };


    // Merge` is deep, so `selection` being an naked object and not a class,
    // will get turned into an immutable Map, necessitating a bunch of ugly
    // `toJS()`/`fromJS()` conversions. The correct thing to do here is make
    // `EditorSelection` a class.
    return state.merge({ expression: newExpression,
                         isInsertingRef: true })
                .set('selection', newSelection);

  } else {
    const { origin } = gridSelection;

    // Until API is turned into actual actions, this is the workaround to avoid dispatch-in-dispatch.
    setTimeout(() => API.evaluate(textboxPosition, expression, currentLanguage), 0);
    const state_ = displayActiveExpression(state, origin);
    return state_;
  }
}

/**
 * Attempts to toggle a ref.
 */
function tryTogglingRef(state: State): State {
  const { selection, expression } = state;
  const { prefix, ref, suffix } = U.Parsing.liftHoveredReference(selection, expression);

  if (ref != null) {
    // toggle the ref, and place cursor after it.
    const newRef = ref.toggle().toString();
    const { row } = U.String.getSelectionLead(selection);
    const newLead = {
      row,
      column: prefix.split('\n').slice(-1)[0].length + newRef.length
    };

    const newExpression = prefix + newRef + suffix;
    const newSelection = {
      range: {start: newLead, end: newLead},
      backwards: false
    };

    return state.set('expression', newExpression)
                .set('selection', newSelection);
  }

  return state;
}

export default new ExpressionStore(dispatcher);
