/* @flow */

import type {
  RangeDescriptor, 
  RangeKey
} from '../types/Eval';

import type {
  ASOverlaySpec
} from '../types/Hypergrid';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';

import U from '../AS/Util';

type RangeDescriptorStoreData = {
  rangeDescriptors: { [key: RangeKey]: RangeDescriptor }; 
};

let _data: RangeDescriptorStoreData = {
  rangeDescriptors: {}
};

const ASRangeDescriptorStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register((action) => {
    logDebug('Range descriptor store received action', action);
    switch (action._type) {
      case 'GOT_UPDATED_RANGE_DESCRIPTORS':
        ASRangeDescriptorStore._removeRangeDescriptorsAt(action.oldRangeKeys);
        ASRangeDescriptorStore._updateRangeDescriptors(action.newRangeDescriptors);
        ASRangeDescriptorStore.emitChange();
        break;
    }
  }),

  _removeRangeDescriptorsAt(rangeKeys: Array<RangeKey>) { 
    rangeKeys.forEach((rk) => {
      if (_data.rangeDescriptors[rk] != null) { 
        delete _data.rangeDescriptors[rk]; 
      }
    });
  },

  _updateRangeDescriptors(rangeDescriptors: Array<RangeDescriptor>) { 
    rangeDescriptors.forEach((rd) => _data.rangeDescriptors[rd.descriptorKey] = rd); 
  },

  getRangeDescriptor(rangeKey: RangeKey): ?RangeDescriptor { 
    return _data.rangeDescriptors[rangeKey];
  }
});

export default ASRangeDescriptorStore;
