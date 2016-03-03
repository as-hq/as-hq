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
      case 'RESET':
        _data.rangeDescriptors = new ObjectDict();
        break;
        
      case 'GOT_UPDATED_RANGE_DESCRIPTORS':
        ASRangeDescriptorStore._removeRangeDescriptorsAt(action.oldRangeKeys);
        ASRangeDescriptorStore._updateRangeDescriptors(action.newRangeDescriptors);
        ASRangeDescriptorStore.emitChange();
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
