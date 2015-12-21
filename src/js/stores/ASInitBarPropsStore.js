/* @flow */

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import Util from '../AS/Util';

import type {
  Bar,
  BarProp
} from '../types/Messages';

let _data = {
  initColumns: ([]: Array<Bar>),
  initRows: ([]: Array<Bar>)
};

const ASInitBarPropsStore = Object.assign({}, BaseStore, {

  /* This function describes the actions of the ASReplStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register(function (action) {
    switch (action._type) {
      case 'GOT_OPEN':
        let initBars = action.initBars,
            initCols = initBars.filter((irc) => irc.barType == 'ColumnType'),
            initRows = initBars.filter((irc) => irc.barType == 'RowType');
        ASInitBarPropsStore._setInitColumns(initCols);
        ASInitBarPropsStore._setInitRows(initRows);
        ASInitBarPropsStore.emitChange();
        break;
    }
  }),

  getInitColumnWidths(): Array<[number,number]> {
    return this._getInitDimensions(_data.initColumns);
  },

  getInitRowHeights(): Array<[number,number]> {
    return this._getInitDimensions(_data.initRows);
  },

  _getInitDimensions(rowsOrCols: Array<Bar>): Array<[number,number]> {
    let dims = []; 
    rowsOrCols.forEach(({barIndex, barProps}) => {
      let dimInd = barProps.map(({tag}) => tag).indexOf('Dimension');
      if (dimInd >= 0) {
        let dimensionProp = barProps[dimInd];
        switch (dimensionProp.tag) {
          case 'Dimension':
            dims.push([barIndex, dimensionProp.contents]);
            break;
        }
      }
    });

    return dims;
  },

  _setInitColumns(initCols: Array<Bar>) {
    _data.initColumns = initCols;
  },

  _setInitRows(initRows: Array<Bar>) {
    _data.initRows = initRows;
  }
});

export default ASInitBarPropsStore;
