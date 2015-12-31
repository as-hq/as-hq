/* @flow */

import type {
  Callback
} from '../types/Base';

import type {InitCallback} from './types';

import Constants from '../Constants';
import ASSpreadsheet from '../components/ASSpreadsheet.jsx';

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

  // make sure the spreadsheet is blank and ready for AlphaSheets to overwrite
  ({ model }) => {
    model.getColumnCount = () => { return Constants.numCols; };
    model.getRowCount = () => { return Constants.numRows; };
    model.getValue = (x, y) => { return ''; };
    model.getCellEditorAt = (x, y) => { return null; };
  }
];

export default convert(callbacks);
