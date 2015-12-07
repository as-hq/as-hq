/* @flow */

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import Util from '../AS/Util';

import type {
  RowCol,
  RowColProp
} from '../types/Messages';

let _data = {
  initColumns: ([]: Array<RowCol>),
  initRows: ([]: Array<RowCol>)
};

const ASInitRowColPropsStore = Object.assign({}, BaseStore, {

  /* This function describes the actions of the ASReplStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register(function (action) {
    switch (action._type) {
      case 'GOT_OPEN':
        let initRowCols = action.initRowCols,
            initCols = initRowCols.filter((irc) => irc.rowColType == 'ColumnType'),
            initRows = initRowCols.filter((irc) => irc.rowColType == 'RowType');
        ASInitRowColPropsStore._setInitColumns(initCols);
        ASInitRowColPropsStore._setInitRows(initRows);
        ASInitRowColPropsStore.emitChange();
        break;
    }
  }),

  getInitColumnWidths(): Array<[number,number]> {
    return this._getInitDimensions(_data.initColumns);
  },

  getInitRowHeights(): Array<[number,number]> {
    return this._getInitDimensions(_data.initRows);
  },

  _getInitDimensions(rowsOrCols: Array<RowCol>): Array<[number,number]> {
    let dims = []; 
    rowsOrCols.forEach(({rowColIndex, rowColProps}) => {
      let dimInd = rowColProps.map(({tag}) => tag).indexOf('Dimension');
      if (dimInd >= 0) {
        let dimensionProp = rowColProps[dimInd];
        switch (dimensionProp.tag) {
          case 'Dimension':
            dims.push([rowColIndex, dimensionProp.contents]);
            break;
        }
      }
    });

    return dims;
  },

  _setInitColumns(initCols: Array<RowCol>) {
    _data.initColumns = initCols;
  },

  _setInitRows(initRows: Array<RowCol>) {
    _data.initRows = initRows;
  }
});

export default ASInitRowColPropsStore;
