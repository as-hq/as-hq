/* @flow */

import type {
  ASClientError,
} from '../types/Errors';

import type {ASAction} from '../types/Actions';
import type {
  ASLanguage,
  ASLocation,
  NakedIndex,
} from '../types/Eval';

import type {
  ASCellGrid,
} from '../types/State';

import {List, Map, Record, Record$Class} from 'immutable';
// $FlowFixMe declare this library
import {ReduceStore} from 'flux/utils';

import Dispatcher from '../Dispatcher';

import U from '../AS/Util';
import API from '../actions/ASApiActionCreators';

import ASCell from '../classes/ASCell';
import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

import Render from '../AS/Renderers';
import GridStore from './ASGridStore.js';
import DescriptorStore from './ASRangeDescriptorStore.js';

/*
Private variable keeping track of a viewing window (cached) of cells. Stores:
  1) All cells, indexed [sheet][col][row]
  2) All errors
  3) Cells that were last updated by an eval or change event (so that
     components can easily access the update from the store)
*/

type CellStoreDataFields = {
  allCells: ASCellGrid;
  allErrors: Array<ASClientError>;
};

type CellStoreData = Record$Class;

const CellStoreDataRecord = Record({
  allCells: Map(),
  allErrors: List(),
});

function removeIndices(
  data: CellStoreData,
  locs: Array<ASIndex>
): CellStoreData {
  let data_ = data;
  locs.forEach((loc) => {
    data_ = removeIndex(data_, loc);
  });
  return data_;
}

// Function to update cell related objects in store.
function updateCells(
  data: CellStoreData,
  cells: Array<ASCell>,
  oldLocs: Array<ASLocation>
): CellStoreData {
  const removedIndices = [];
  let data_ = data;
  cells.forEach(cell => {
    if (!cell.isEmpty()) {
      data_ = setCell(data_, cell);
    } else {
      // filter out all the blank cells passed back from the store
      removedIndices.push(cell.location);
    }
  });
  data_ = removeIndices(data_, removedIndices);
  return removeLocations(data_, oldLocs);
}

// Remove cells at a list of ASLocation's.
function removeLocations(
  data: CellStoreData,
  locs: Array<ASLocation>
): CellStoreData {
  let data_ = data;
  U.Location.asLocsToASIndices(locs).forEach(i => {
    data_ = removeIndex(data_, i);
  });
  return data_;
}

function locationExists(data: CellStoreData, index: ASIndex): boolean {
  const {col, row, sheetId} = index;
  return data.getIn(['allCells', sheetId, col, row], false);
}

type LanguageAndExpression = {
  language: ASLanguage;
  expression: string;
};

class ASCellStore extends ReduceStore<CellStoreData> {
  getInitialState(): CellStoreData {
    return new CellStoreDataRecord();
  }

  reduce(state: CellStoreData, action: ASAction): CellStoreData {
    switch (action._type) {

      case 'SHEET_UPDATED': {
        const {update: {cellUpdates}} = action;

        if (! U.Conversion.updateIsEmpty(cellUpdates)) {
          const newCells =  ASCell.makeCells(cellUpdates.newVals);
          const oldLocs = U.Location.makeLocations(cellUpdates.oldKeys);
          return updateCells(state, newCells, oldLocs);
        }
        return state;
      }

      case 'CLEARED': {
        return state.set('allCells', Map());
      }

      case 'CLEARED_SHEET': {
        return state.setIn(
          ['allCells', action.sheetId],
          Map()
        );
      }

      default:
        return state;
    }
  }

  getDisplayedValueAt(idx: ASIndex): string {
    const cell = this.getCell(idx);
    if (!! cell) {
      return U.Render.showValue(cell.value).toString();
    } else {
      return '';
    }
  }

  getActiveCell(): ?ASCell {
    const {origin} = GridStore.getActiveSelection();
    return this.getCell(origin);
  }

  getActiveCells(): Array<?ASCell> {
    const {range} = GridStore.getActiveSelection();
    return this.getCells(range);
  }

  activeCellHasError(): boolean {
    const cell = this.getActiveCell();
    return !!cell ? cell.hasError() : false;
  }

  activeCellHasOutput(): boolean {
    const cell = this.getActiveCell();
    return !!cell ? cell.hasOutput() : false;
  }

  activeCellIsObject(): boolean {
    const cell = this.getActiveCell();
    return !!cell ? cell.isObject() : false;
  }

  getActiveCellDisplay(): ?string {
    const cell = this.getActiveCell();
    return !!cell ? cell._display : null;
  }

  // Converts a range to a row major list of lists of values, represented by
  // their underlying strings.
  getRowMajorCellValues(rng: ASRange): Array<Array<string>> {
    const {tl, br} = rng;
    const height = br.row - tl.row + 1;
    const length = br.col - tl.col + 1;
    const rowMajorValues = U.Array.make2DArrayOf('', height, length);

    for (let i = 0; i < height; ++i) {
      const currentRow = tl.row + i;
      rowMajorValues[i] = rowMajorValues[i].map((value, index) => {
        const currentColumn = tl.col + index;
        return this.getDisplayedValueAt(ASIndex.fromNaked({
          col: currentColumn,
          row: currentRow
        }));
      });
    }
    return rowMajorValues;
  }

  getAllErrors(): Array<ASClientError> {
    return this.getState().allErrors;
  }

  isNonBlankCell(idx: ASIndex): boolean {
    const {col, row, sheetId} = idx;
    const data = this.getState();

    return (
      locationExists(data, idx) &&
      data.getIn(['allCells', sheetId, col, row]).expression.expression != ''
    );
  }

  getCells(rng: ASRange): Array<?ASCell> {
    return rng.toIndices().map((loc) => this.getCell(loc));
  }

  getCells2D(rng: ASRange): Array<Array<?ASCell>> {
    return U.Array.map2d(rng.toIndices2d(), loc => this.getCell(loc));
  }

  getCell(loc: ASIndex): ? ASCell {
    const data = this.getState();
    return locationExists(data, loc)
      ? data.getIn(['allCells', loc.sheetId, loc.col, loc.row])
      : null;
  }

  getAllCells(sheetId: string): Array<ASCell> {
    const sheet = this.getState().allCells.get(sheetId);
    const grid = sheet.toArray().map(r => r.toArray());
    return [].concat(...grid);
  }

  getValues2D(sheetId: string): Array<Array<String>> {
    return _.range(1,26).map(col => _.range(1,1000).map(row => {
      const idx = ASIndex.fromNaked({row, col}, sheetId);
      const mcell = this.getCell(idx);
      return mcell ? U.Render.showValue(mcell.value) : '';
    }));
  }

  getHtml(sheetId: string) {
    const vs = this.getValues2D();

    let body = "<table>";
    for(let i = 0; i < vs.length; i++) {
      body += "<tr>";
      for(let j = 0; j < vs[i].length; j++){
        body += "<td width=\"60px\">" + vs[i][j] + "</td>";
      }
      body += "</tr>";
    }
    body += "</table>";
    return body;
  }

  /**************************************************************************************************************************/
  /* Data boundaries */

  getDataBoundary(start: ASIndex, direction: string): ASIndex {
    let dY = 0, dX = 0;

    switch (direction) {
      case "Right": dX = 1; break;
      case "Left": dX = -1; break;
      case "Down": dY = 1; break;
      case "Up": dY = -1; break;
      default: throw "Invalid direction passed in";
    }

    const shiftAmount = { dY: dY, dX: dX };
    let prev = start;
    let curIdx = start.shift(shiftAmount);
    let next = curIdx.shift(shiftAmount);

    // go in this direction (shiftAmount)
      // find the first one that's a transition or an edge

    function checkWhetherCurrentIsBoundary (p, c, n) {
      return c && !(p && n);
    };

    while (!curIdx.equals(next)) { // while you still have a next, and you haven't reached boundary
      const [p, c, n] = [prev, curIdx, next].map(i => this.isNonBlankCell(i));
      if (checkWhetherCurrentIsBoundary(p, c, n)) {
        break;
      }
      [prev, curIdx, next] = // move to the next window of 3 cells
        [prev, curIdx, next].map((x) => x.shift(shiftAmount));
    }

    return curIdx;
  }

  //This function returns what the new selection would be if you pressed ctrl+shift+right/up/left/down.
  //If shift is not held down,
  getDataBoundSelection(selection: ASSelection, direction: string): ASSelection {
    const {range: {tl, br}, origin} = selection;

    let startLoc = { row: origin.row, col: origin.col };
    switch (direction) {
      case "Right": startLoc.col = (origin.col == tl.col) ? br.col : tl.col; break;
      case "Left": startLoc.col = (origin.col == br.col) ? tl.col : br.col; break;
      case "Up": startLoc.row = (origin.row == br.row) ? tl.row : br.row; break;
      case "Down": startLoc.row = (origin.row == tl.row) ? br.row : tl.row; break;
      default: throw "Invalid direction passed in";
    }

    const bound = this.getDataBoundary(ASIndex.fromNaked(startLoc), direction);

    // slight misnomers; these are the corners, but not necessarily top left or bottom right
    const newTl = {row: tl.row, col: tl.col};
    const newBr = {row: br.row, col: br.col};
    if (direction == "Up" || direction == "Down") {
      if (origin.row > tl.row)
        newTl.row = bound.row;
      else
        newBr.row = bound.row;
    } else if (direction == "Left" || direction == "Right") {
      if (origin.col > tl.col)
        newTl.col = bound.col;
      else
        newBr.col = bound.col;
    }
    // I haven't actually figured out why the above code works, it seems like it sort of just does.

    return new ASSelection({ range: {tl: newTl, br: newBr}, origin });
  }

}

function unsetErrors(data: CellStoreData, cell: ASCell) {
  return data.update('allErrors', allErrors => allErrors.filter(
    ({location}) => ! cell.location.equals(location)
  ));
}

function setErrors(data: CellStoreData, cell: ASCell) {
  const data_ = unsetErrors(data, cell);
  const {value: cv, expression: cxp, location: cl} = cell;

  switch (cv.tag) {
    case 'ValueError':
      return data_.update('allErrors', errors => errors.push({
        location: cl,
        language: cxp.language,
        msg: cv.errorMsg,
      }));
    default:
      return data_;
  }
}

function setCell(data: CellStoreData, cell: ASCell) {
  const {col, row, sheetId} = cell.location;
  let data_ = data;
  if (API.isTesting) {
    // XXX this should definitely not need to happen according to Immutable's API
    // but when testing (and ONLY then), if you don't initialize this path with an empty Map,
    // immutable throws an invalid keyPath error.
    data_ = data_.setIn(['allCells', sheetId], Map());
  }
  data_ = data_.setIn(['allCells', sheetId, col, row], cell);
  return setErrors(data_, cell);
}

// Remove a cell at an ASIndex
function removeIndex(data: CellStoreData, loc: ASIndex): CellStoreData {
  let data_ = data;
  const emptyCell = ASCell.emptyCellAt(loc);
  if (locationExists(data_, loc)) {
    data_ = data_.deleteIn(['allCells', loc.sheetId, loc.col, loc.row]);
  }

  return unsetErrors(data_, emptyCell);
}

// A lot of things listen to this store, eventemitter think's there's a memory
// leak
// TODO(joel) - is this an eventemitter? how does this work?
// ASCellStore.setMaxListeners(100);
//
export default new ASCellStore(Dispatcher);
