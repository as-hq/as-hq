// @flow
// Focus Store - For remembering where focus should return
//
// This store remembers the last two places (of grid, textbox, and editor) that
// were focused and support refocusing on the last focused element (after a
// button click, for instance) or toggling between the last focuses with F2.

import type {ASFocusType, FocusStoreCallbacks} from '../types/State';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';

type FocusStoreData = {
  activeFocus: ASFocusType;
  lastActiveFocus: ASFocusType;
  callbacks: FocusStoreCallbacks | null;
};

const _data: FocusStoreData = {
  activeFocus: 'grid',
  lastActiveFocus: 'textbox',
  callbacks: null,
};

const dispatcherIndex = Dispatcher.register(action => {
  switch (action._type) {
    case 'FOCUSED': {
      setFocus(action.focus);
      FocusStore.emitChange();
      break;
    }

    case 'SET_FOCUS_CALLBACKS': {
      _data.callbacks = action.callbacks;
      FocusStore.emitChange();
      break;
    }

    case 'TOGGLED_FOCUS_F2': {
      // F2 toggles back and forth between the grid and either editor or textbox
      // depending on which was last focused.
      //
      //     Editor
      //      v ^
      //     Grid
      //      v ^
      //     Textbox
      const temp = _data.activeFocus;

//       switch(`${_data.activeFocus}<-${data.lastActiveFocus}`) {
//         // TODO(joel) - how can both recent focuses be grid?
//         // case 'grid<-grid':
//         case 'grid<-textbox':
//           _data.activeFocus = 'textbox';
//           break;
//         case 'grid<-editor':
//           _data.activeFocus = 'editor';
//           break;
//         case 'textbox<-grid':
//         case 'textbox<-editor':
//           _data.activeFocus = 'grid';
//           break;
//         case 'editor<-grid':
//         case 'editor<-textbox':
//           _data.activeFocus = 'grid';
//           break;
//       }

      if (_data.activeFocus === 'grid') {
        if (_data.lastActiveFocus === 'grid') {
          _data.activeFocus = 'textbox';
        } else {
          // toggle back to the previous thing
          _data.activeFocus = _data.lastActiveFocus;
        }

      // TODO(joel): only switch to the grid if in a parsable position
      // const x = CellStore.getActiveCell();
      // const y = CellStore.cellToJSVal(x);
      } else { // if (Util.Parsing.canInsertCellRefInXp(y + '')) {
        _data.activeFocus = 'grid';
      }

      _data.lastActiveFocus = temp;
      FocusStore.emitChange();
      break;
    }

    // TODO(joel): I don't understand why we focus on GOT_UPDATED_CELLS
    case 'GOT_UPDATED_CELLS':
    case 'NORMAL_SEL_CHANGED':
      setFocus('grid');
      FocusStore.emitChange();
      break;

    default:
      break;
  }
});

const FocusStore = Object.assign({}, BaseStore, {
  dispatcherIndex,

  refocus() {
    if (_data.callbacks !== null) {
      _data.callbacks[_data.activeFocus]();
    }
  },
  getFocus() {
    return _data.activeFocus;
  },

});

function setFocus(focus: ASFocusType) {
  _data.lastActiveFocus = _data.activeFocus;
  _data.activeFocus = focus;

  // actually set the focus
  FocusStore.refocus();
}

export default FocusStore;
