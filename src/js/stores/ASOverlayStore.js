/* @flow */

import type {
} from '../types/Eval';

import type {
  ASOverlaySpec
} from '../types/Hypergrid';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';

import U from '../AS/Util';

/*
Private variable keeping track of all overlays added to the sheet.
*/

type OverlayStoreData = {
  overlays: { [keys: string]: ASOverlaySpec };
};

let _data: OverlayStoreData = {
  overlays: {}
};

const ASOverlayStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register((action) => {
    logDebug('Store received action', action);
    switch (action._type) {
      case 'ADD_OVERLAY':
        ASOverlayStore.add(action.overlay);
        ASOverlayStore.emitChange();
        break;
    }
  }),

  add(ovl: ASOverlaySpec) {
    _data.overlays[ovl.id] = ovl;
  },

  getAll(): Array<ASOverlaySpec> {
    return Object.keys(_data.overlays).map((key) => _data.overlays[key]);
  },

  deleteById(id: string) {
    delete _data[id];
  }
});

export default ASOverlayStore;
