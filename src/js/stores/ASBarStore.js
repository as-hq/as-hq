/* @flow */

// Bar means row or col

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import Util from '../AS/Util';

import type {
  Bar,
  BarProp,
  BarIndex,
  BarType,
} from '../types/Messages';

import type {
  ASBarLines
} from '../types/State';

let _data = {
  bars: ({}: ASBarLines),
  lastUpdatedBars: ([]: Array<Bar>)
};

// Dict that takes in a RowType/ColumnType and maps to ordered pairs of row/col indices and their heights/widths (null if not set)
type BarDimensions = { [key: BarType]: Array<[number,?number]> }; 

const ASBarStore = Object.assign({}, BaseStore, {

  /* This function describes the actions of the ASReplStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register(function (action) {
    switch (action._type) {
      case 'GOT_UPDATED_BARS':
        _data.lastUpdatedBars = []; 

        let newBars = action.newBars;
        ASBarStore._updateBars(newBars);

        let oldBarLocs = action.oldBarLocs;
        ASBarStore._removeBarsAt(oldBarLocs);

        ASBarStore.emitChange();
        break;
    }
  }),

  _getDimensions(bars: Array<Bar>): BarDimensions {
    let dims = {'RowType': [], 'ColumnType': []};
    bars.forEach(({barIndex, barProps}) => {
      let dimInd = barProps.map(({tag}) => tag).indexOf('Dimension');
      if (dimInd >= 0) {
        let dimensionProp = barProps[dimInd];
        switch (dimensionProp.tag) { // so this flows
          case 'Dimension':
            dims[barIndex.barType].push([barIndex.barNumber, dimensionProp.contents]);
            break;
        }
      } else { 
        dims[barIndex.barType].push([barIndex.barNumber, null]);
      }
    });

    return dims;
  },

  getLastUpdatedBarsDimensions(): BarDimensions { 
    return ASBarStore._getDimensions(_data.lastUpdatedBars);
  },

  _updateBars(bars: Array<Bar>) {
    bars.forEach((b) => _data.bars[b.barIndex] = b); 
    _data.lastUpdatedBars = _data.lastUpdatedBars.concat(bars);
  },

  _removeBarsAt(barInds: Array<BarIndex>) {
    barInds.forEach((i) => {
      if (_data.bars[i] != null) {
        delete _data.bars[i]; 
      }
    });

    _data.lastUpdatedBars = _data.lastUpdatedBars.concat(ASBarStore._blankBarsAt(barInds));
  },

  _blankBarsAt(inds: Array<BarIndex>): Array<Bar> { 
    return inds.map((i) => {
      return {
        tag: 'Bar', 
        barIndex: i, 
        barProps: []
      };
    });
  }
});

export default ASBarStore;
