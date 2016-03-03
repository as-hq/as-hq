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

import Render from '../AS/Renderers';
import ExpressionStore from './ASExpressionStore';
import SelectionStore from './ASSelectionStore.js';
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

class ASCellStore extends ReduceStore<CellStoreData> {
  getInitialState(): CellStoreData {
    return new CellStoreDataRecord();
  }

  reduce(state: CellStoreData, action: ASAction): CellStoreData {
    switch (action._type) {
      case 'GOT_UPDATED_CELLS': {
        // Wait for range descriptors to be updated.
        Dispatcher.waitFor([DescriptorStore.dispatcherIndex]);
        return updateCells(state, action.newCells, action.oldLocs);
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

      case 'RESET':
        return this.getInitialState();

      default:
        return state;
    }
  }

  getDisplayedValueAt(idx: ASIndex): string {
    const cell = this.getCell(idx);
    if (!! cell) {
      return U.Render.showValue(cell.value);
    } else {
      return '';
    }
  }

  getActiveCell(): ?ASCell {
    const {origin} = SelectionStore.getActiveSelection();
    return getCell(this.getState(), origin);
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

  getCells(rng: ASRange): Array<Array<?ASCell>> {
    const data = this.getState();
    return U.Array.map2d(rng.toIndices2d(), loc => getCell(data, loc));
  }

  getCell(loc: ASIndex): ? ASCell {
    return getCell(this.getState(), loc);
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
    data_ = removeIndex(data, i);
  });
  return data_;
}

function locationExists(data: CellStoreData, index: ASIndex): boolean {
  const {col, row, sheetId} = index;
  return data.getIn(['allCells', sheetId, col, row], false);
}

function getCell(data: CellStoreData, loc: ASIndex): ?ASCell {
  return locationExists(data, loc)
    ? data.getIn(['allCells', loc.sheetId, loc.col, loc.row])
    : null;
}

type LanguageAndExpression = {
  language: ASLanguage;
  expression: string;
};

function waitForLanguageAndExpression(): LanguageAndExpression {
  Dispatcher.waitFor([ExpressionStore.getDispatchToken()]);
  return {
    language: ExpressionStore.getLanguage(),
    expression: ExpressionStore.getExpression(),
  };
}

// A lot of things listen to this store, eventemitter think's there's a memory
// leak
// TODO(joel) - is this an eventemitter? how does this work?
// ASCellStore.setMaxListeners(100);
//
export default new ASCellStore(Dispatcher);
