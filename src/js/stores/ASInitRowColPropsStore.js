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
            initCols = initRowCols.filter((irc) => irc.rowColType == 'ColumnType');
        ASInitRowColPropsStore.setInitColumns(initCols);
        ASInitRowColPropsStore.emitChange();
        break;
    }
  }),

  getInitColumns(): Array<RowCol> {
    return _data.initColumns;
  },

  getInitColumnWidths(): Array<[number,number]> {
    let columnWidths = []; 
    _data.initColumns.forEach(({rowColIndex, rowColProps}) => {
      let dimInd = rowColProps.map(({tag}) => tag).indexOf('Dimension');
      if (dimInd >= 0) {
        let dimensionProp = rowColProps[dimInd];
        switch (dimensionProp.tag) {
          case 'Dimension':
            columnWidths.push([rowColIndex, dimensionProp.contents]);
            break;
        }
      }
    });

    return columnWidths;
  },

  setInitColumns(initCols: Array<RowCol>) {
    _data.initColumns = initCols;
  }
});

export default ASInitRowColPropsStore;
