/* @flow */
import React from 'react';
import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';
import Util from '../AS/Util';
import {logDebug} from '../AS/Logger';
import Render from '../AS/Render';
import SheetStateStore from './ASSheetStateStore';
import CellStore from './ASCellStore';
import TC from '../AS/TypeConversions';

import type {
  ASLanguage
} from '../types/Eval';

import type {
  ASSelection,
} from '../types/State';

type SelectionStoreData = {
  activeSelection: ?ASSelection;
};

let _data: SelectionStoreData = {
  activeSelection: null
};

const ASSelectionStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register((action) => {
    switch (action._type) {
      case 'GOT_SELECTION':
        ASSelectionStore.setActiveSelection(TC.asSelectionToSimple(action.newSelection), "", null);
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
    let origin = sel.origin;
    _data.activeSelection = sel;
    SheetStateStore.setActiveCell(CellStore.getCell(origin.col, origin.row) || TC.makeEmptyCell());
    var activeCellDependencies = Util.parseDependencies(xp, lang);
    let c = sel.origin.col,
        r = sel.origin.row,
        listDep = CellStore.getParentList(c, r);
    if (listDep !== null) {
      activeCellDependencies.push(listDep);
    }
    SheetStateStore.setActiveCellDependencies(activeCellDependencies);
    ASSelectionStore.emitChange();
  },

  getActiveSelection(): ?ASSelection {
    return _data.activeSelection;
  }
});

export default ASSelectionStore;
