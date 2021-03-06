/* @flow */

import type {
  RangeDescriptor,
  RangeKey
} from '../types/Eval';

import type {
  ASOverlaySpec
} from '../types/Overlay';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';

import U from '../AS/Util';

import ObjectDict from '../classes/ObjectDict';

type RangeDescriptorStoreData = {
  rangeDescriptors: ObjectDict<RangeKey, RangeDescriptor>;
};

let _data: RangeDescriptorStoreData = {
  rangeDescriptors: new ObjectDict()
};

const ASRangeDescriptorStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register((action) => {
    logDebug('Range descriptor store received action', action);
    switch (action._type) {
      case 'CLEARED_SHEET':
        _data.rangeDescriptors = new ObjectDict();
        break;

      case 'SHEET_UPDATED':
        const {update: {descriptorUpdates}} = action;
        if (! U.Conversion.updateIsEmpty(descriptorUpdates)) {
          ASRangeDescriptorStore._removeRangeDescriptorsAt(descriptorUpdates.oldKeys);
          ASRangeDescriptorStore._updateRangeDescriptors(descriptorUpdates.newVals);
          ASRangeDescriptorStore.emitChange();
        }
        break;
    }
  }),

  _removeRangeDescriptorsAt(rangeKeys: Array<RangeKey>) {
    rangeKeys.forEach((rk) => {
      if (_data.rangeDescriptors.get(rk) != null) {
        _data.rangeDescriptors.del(rk);
      }
    });
  },

  _updateRangeDescriptors(rangeDescriptors: Array<RangeDescriptor>) {
    rangeDescriptors.forEach((rd) => {
      _data.rangeDescriptors.set(rd.descriptorKey, rd);
    });
  },

  getRangeDescriptor(rangeKey: RangeKey): ?RangeDescriptor {
    return _data.rangeDescriptors.get(rangeKey);
  }
});

export default ASRangeDescriptorStore;
