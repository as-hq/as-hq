/* @flow */

import Dispatcher from '../Dispatcher';
import ASSelection from '../classes/ASSelection';
import Render from '../AS/Renderers';
import BaseStore from './BaseStore';
import {Just} from '../AS/Maybe';

type SelectionStoreData = {
  activeSelection: ?ASSelection;
};

let _data: SelectionStoreData = {
  activeSelection: null
};

const ASSelectionStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register((action) => {
    switch (action._type) {
      case 'RESET':
        _data.activeSelection = null;
        break;
      case 'GOT_SELECTION':
        setActiveSelection(action.newSelection);
        ASSelectionStore.emitChange();
        break;
      case 'SET_ACTIVE_SELECTION':
        setActiveSelection(action.selection);
        ASSelectionStore.emitChange();
        break;
    }
  }),

  /**************************************************************************************************************************/
  /* getter and setter methods */
  getActiveSelection(): ?ASSelection {
    return _data.activeSelection;
  },

  withActiveSelection<T>(cb: (sel: ASSelection) => T): ?T {
    return Just(ASSelectionStore.getActiveSelection()).fmap(cb).out();
  },
});


// A lot of things listen to this store, eventemitter think's there's a memory
// leak
ASSelectionStore.setMaxListeners(100);


function setActiveSelection(sel: ASSelection) {
  // Render.setSelection() is for speed purposes only. Ideally we would be
  // getting the selection from this store during render, but getting the
  // variable from the store is empirically much slower than just setting
  // its value directly in the file. (Relayed from Anand -- Alex 12/9)
  Render.setSelection(sel);
  _data.activeSelection = sel;
}


export default ASSelectionStore;
