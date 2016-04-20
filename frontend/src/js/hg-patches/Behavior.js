/* @flow */

import type {
  Callback
} from '../types/Base';

import type {InitCallback} from './types';

import Constants from '../Constants';
import ASSpreadsheet from '../components/ASSpreadsheet.jsx';
import CellStore from '../stores/ASCellStore';
import ASIndex from '../classes/ASIndex';
import U from '../AS/Util';

import {convert} from './helpers';

const callbacks: Array<InitCallback> = [
  // set global properties directly
  ({ hg }) => {
    hg.autoScrollAcceleration = false;
  },

  // set indirect global props, because Steve made them separate
  ({ hg, spreadsheet }) => {
    hg.addGlobalProperties(spreadsheet.gridProperties);
  },

  // This overrides the swapping of columns in hypergrid's internal state
  // Keeps the animation, but don't change state = column headers stay same, data stays same
  ({ model }) => {
    model.swapColumns = (src, tar) => {};
  },

  // Cells as links.
  ({ model }) => {
    model.getCursorAt = (x, y) => {
      if (U.String.isLink(model.getValue(x,y))) {
        return 'pointer';
      }
      return null;
    };
  },

  // Disable hover grey-out.
  ({ model }) => {
    model.highlightCellOnHover = (isC, isH) => false;
  },

  // The underlying hypergrid model will auto-reflect data in CellStore
  ({ model }) => {
    model.getColumnCount = () => { return Constants.numCols; };
    model.getRowCount = () => { return Constants.numRows; };
    model.getValue = (x, y) => CellStore.getDisplayedValueAt(ASIndex.fromNaked({
      col: x + 1,
      row: y + 1
    }));
    model.getCellEditorAt = (x, y) => { return null; };
  }
];

export default convert(callbacks);
