/* @flow */

// Bar means row or col

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
  columns: ([]: Array<Bar>),
  rows: ([]: Array<Bar>)
};

const ASInitBarPropsStore = Object.assign({}, BaseStore, {

  /* This function describes the actions of the ASReplStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register(function (action) {
    switch (action._type) {
      case 'GOT_OPEN':
        let initBars = action.initBars,
            cols = initBars.filter((irc) => irc.barIndex.barType == 'ColumnType'),
            rows = initBars.filter((irc) => irc.barIndex.barType == 'RowType');
        ASInitBarPropsStore._setColumns(cols);
        ASInitBarPropsStore._setRows(rows);
        ASInitBarPropsStore.emitChange();
        break;
    }
  }),

  // for each col with a width prop, returns (col number, width)
  getColumnWidths(): Array<[number,number]> {
    return this._getInitDimensions(_data.columns);
  },

  // for each row with a height prop, returns (row number, height)
  getRowHeights(): Array<[number,number]> {
    return this._getInitDimensions(_data.rows);
  },

  _getInitDimensions(rowsOrCols: Array<Bar>): Array<[number,number]> {
    let dims = []; 
    rowsOrCols.forEach(({barIndex, barProps}) => {
      let dimInd = barProps.map(({tag}) => tag).indexOf('Dimension');
      if (dimInd >= 0) {
        let dimensionProp = barProps[dimInd];
        switch (dimensionProp.tag) { // so this flows
          case 'Dimension':
            dims.push([barIndex.barNumber, dimensionProp.contents]);
            break;
        }
      }
    });

    return dims;
  },

  _setColumns(cols: Array<Bar>) {
    _data.columns = cols;
  },

  _setRows(rows: Array<Bar>) {
    _data.rows = rows;
  }
});

export default ASInitBarPropsStore;
