/* @flow */

import type {
  Maybe
} from '../AS/Maybe';

import React from 'react';
import Dispatcher from '../Dispatcher';

import U from '../AS/Util';

import {logDebug} from '../AS/Logger';
import Render from '../AS/Renderers';

import BaseStore from './BaseStore';
import SheetStateStore from './ASSheetStateStore';
import CellStore from './ASCellStore';

import {Just} from '../AS/Maybe';

import type {
  ASLanguage,
  ASSelectionObject
} from '../types/Eval';

import type {
  Callback
} from '../types/Base';

type SelectionStoreData = {
  activeSelection: ?ASSelectionObject;
};

let _data: SelectionStoreData = {
  activeSelection: null
};

const ASSelectionStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register((action) => {
    switch (action._type) {
      case 'GOT_SELECTION':
        ASSelectionStore.setActiveSelection(action.newSelection, "", null);
        break;
    }
  }),

  /**************************************************************************************************************************/
  /* getter and setter methods */

  setActiveSelection(sel, xp, lang: ?ASLanguage) {
    // Render.setSelection() is for speed purposes only. Ideally we would be
    // getting the selection from this store during render, but getting the
    // variable from the store is empirically much slower than just setting
    // its value directly in the file. (Relayed from Anand -- Alex 12/9)
    Render.setSelection(sel);
    let origin = sel.origin,
        activeCellDependencies = U.Parsing.parseDependencies(xp, lang),
        listDep = CellStore.getParentList(origin);
    _data.activeSelection = sel;
    if (listDep !== null) {
      activeCellDependencies.push(listDep);
    }
    CellStore.setActiveCellDependencies(activeCellDependencies);
    ASSelectionStore.emitChange();
  },

  getActiveSelection(): ?ASSelectionObject {
    return _data.activeSelection;
  },

  withActiveSelection<T>(cb: (sel: ASSelectionObject) => T): ?T {
    return Just(ASSelectionStore.getActiveSelection()).fmap(cb).out();
  },
});

export default ASSelectionStore;
