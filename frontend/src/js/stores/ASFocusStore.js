// @flow
// Focus Store - remembers focus for all components under the control of Focusable
//
// This store remembers the last two places (of grid, textbox, and editor) that
// were focused and support refocusing on the last focused element (after a
// button click, for instance) or toggling between the last focuses with F2.

import type { FocusedElement } from '../types/State';
import type { ASAction } from '../types/Actions';

import Immutable from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';
import dispatcher from '../Dispatcher';

import ExpressionStore from './ASExpressionStore';
import ConfigStore from './ASConfigurationStore';

import U from '../AS/Util';

type State = any;
const StateRecord = Immutable.Record({
  activeFocus: 'grid',
  lastActiveFocus: 'textbox',
  activeHover: 'grid',
  textboxHasFullFocus: false
});

class FocusStore extends ReduceStore<State> {
  getInitialState(): State {
    return new StateRecord();
  }

  reduce(state: State, action: ASAction): State {
    switch(action._type) {

      case 'FOCUSED': {
        const { focus } = action;
        console.error('focusing element:', focus);

        // focus-stealing prevention
        if (isFocusTheft(state, focus)) {
          this.refocus();
          return state;
        } else {
          return setFocus(state, focus);
        }
      }

      case 'HOVERED': {
        return state.set('activeHover', action.hover);
      }

      case 'FOCUSED_TEXTBOX_FULLY': {
        return setFocus(state, 'textbox').set('textboxHasFullFocus', true);
      }

      case 'TOGGLED_FOCUS_F2': {
        if (state.activeFocus === 'textbox') {
          if (state.lastActiveFocus === 'grid') {
            return state.update('textboxHasFullFocus', b => !b);
          } else if (state.lastActiveFocus === 'editor') {
            return setFocus(state, 'editor');
          }
        } else if (state.activeFocus === 'editor') {
          if (state.lastActiveFocus === 'grid') {
            return setFocus(state, 'textbox')
                  .set('textboxHasFullFocus', false);
          } else if (state.lastActiveFocus === 'textbox') {
            return setFocus(state, 'textbox')
                  .set('textboxHasFullFocus', true);
          }
        }
        return state;
      }

      case 'START_EDITING': {
        console.warn('start editing!');
        return setFocus(state, 'textbox')
              .set('textboxHasFullFocus', action.textboxHasFullFocus);
      }

      case 'STOP_EDITING':
      case 'API_EVALUATE': {
        return setFocus(state, 'grid');
      }

      case 'SELECTION_CHANGED': {
        this.getDispatcher().waitFor([ExpressionStore.getDispatchToken()]);

        if (! ExpressionStore.isEditing()) {
          return setFocus(state, 'grid');
        } else {
          this.refocus();
          return state;
        }
      }

      case 'HEADER_TOGGLED': {
        // wait for header to open, then set its focus.
        this.getDispatcher().waitFor([ConfigStore.getDispatchToken()]);

        if (ConfigStore.isHeaderOpen()) {
          return setFocus(state, 'header');
        // if already open (and now closing), return focus to the last element.
        } else if (state.activeFocus === 'header') {
          return returnFocus(state);
        }
      }

      default:
        return state;
    }
  }

  refocus() {
    this.__emitChange();
  }

  isFocused(elem: FocusedElement): boolean {
    return this.getState().activeFocus === elem;
  }

  getFocus(): FocusedElement {
    return this.getState().activeFocus;
  }

  getHover(): FocusedElement {
    return this.getState().activeHover;
  }

  textboxHasFullFocus(): boolean {
    return this.getState().textboxHasFullFocus;
  }
}

function returnFocus(state: State): State {
  return setFocus(state, state.lastActiveFocus);
}

function setFocus(state: State, focus: FocusedElement): State {
  const lastActiveFocus =
    (focus === state.activeFocus) ?
    state.lastActiveFocus :
    state.activeFocus;
  const textboxHasFullFocus = (focus === 'textbox') ? state.textboxHasFullFocus : false;

  return new StateRecord({
    activeFocus: focus,
    lastActiveFocus,
    textboxHasFullFocus
  });
}

function isFocusTheft(state: State, focus: FocusedElement): boolean {
  return (
    /**
     * Don't steal focus from the editors when editing;
     * the SELECTION_CHANGED action takes care of passing
     * focus back to the grid when it is correct to do so.
     * E.g. when not in a ref-insertable position.
     */
    ExpressionStore.isEditing() && focus === 'grid' ||

    /**
     * Tab causes textareas to give away focus;
     * return focus to the original.
     */
    (
      ExpressionStore.getExpression().slice(-1) === '\t' &&
      focus === 'textbox' &&
      state.activeFocus === 'editor'
    )
  );
}

export default new FocusStore(dispatcher);
