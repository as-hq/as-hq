/* @flow */

import type {
  ASClientError,
} from '../types/Errors';

import type {ASAction} from '../types/Actions';
import type {
  ASLanguage,
  ASLocation,
} from '../types/Eval';

import type {
  ASCellGrid,
} from '../types/State';

import {List, Map, Record, Record$Class} from 'immutable';
// $FlowFixMe declare this library
import {ReduceStore} from 'flux/utils';

import Dispatcher from '../Dispatcher';

import U from '../AS/Util';

import ASCell from '../classes/ASCell';
import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';

import Render from '../AS/Renderers';
import ExpStore from './ASExpStore';
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
  lastUpdatedCells: Array<ASCell>;
};

type CellStoreData = Record$Class;

const CellStoreDataRecord = Record({
  allCells: Map(),
  allErrors: List(),
  lastUpdatedCells: List(),
});

class ASCellStore extends ReduceStore<CellStoreData> {
  getInitialState(): CellStoreData {
    return new CellStoreDataRecord();
  }

  reduce(state: CellStoreData, action: ASAction): CellStoreData {
    switch (action._type) {
      case 'GOT_UPDATED_CELLS': {
        // The expanding types that we put into the cells depends on the
        // updated range descriptors
        Dispatcher.waitFor([DescriptorStore.dispatcherIndex]);

        let state_ = state.set('lastUpdatedCells', List());
        state_ = removeLocations(state_, action.oldLocs);
        return updateCells(state_, action.newCells);
      }

      /*
        The server has cleared everything from the DB
        Need to delete the store
        Called from Dispatcher, fired by API response from server
      */
      case 'CLEARED': {
        let state_ = state.set('lastUpdatedCells', List());

        let cellsToRemove = [];
        function getCellsToRemove(colArray) {
          colArray.forEach((cell) => {
            cellsToRemove.push(cell);
          });
        }

        for (const s of state_.allCells) {
          state_.getIn(['allCells', s]).forEach(getCellsToRemove);
        }

        // remove possibly null cells
        cellsToRemove = cellsToRemove.filter(cell => !!cell);

        state_ = removeCells(state_, cellsToRemove);
        return state_.set('allCells', Map());
      }

      case 'CLEARED_SHEET': {
        let state_ = state.set('lastUpdatedCells', List());
        const {sheetId} = action;

        if (state_.getIn(['allCells', sheetId])) {
          let cr = [];
          state_.allCells.get(sheetId).forEach((colArray) => {
            colArray.forEach((cell) => {
              cr.push(cell);
            });
          });

          // remove possibly null cells
          cr = cr.filter((cell) => !!cell);

          state_ = removeCells(state_, cr);
          state_ = state_.setIn(['allCells', sheetId], []);
        }

        return state_;
      }

      // Dependency highlighting temporarily disabled until ExpStore is
      // refactored. (anand 2/15)
      //
      // case 'TEXTBOX_CHANGED':
      // case 'GRID_KEY_PRESSED': {
      //   Dispatcher.waitFor([SelectionStore.dispatcherIndex]);
      //   SelectionStore.withActiveSelection({origin} => {
      //     const {language} = waitForLanguageAndExpression();
      //     const deps = U.Parsing.parseDependencies(action.xpStr, language);
      //     Render.setDependencies(deps);
      //   })
      //   break;
      // }

      default:
        return state;
    }
  }

  getActiveCell(): ?ASCell {
    return SelectionStore.withActiveSelection(({origin}) => {
      return getCell(this.getState(), origin);
    });
  }

  getActiveCellDisplay(): ?string {
    const cell = this.getActiveCell();
    return !!cell ? cell._display : null;
  }

  // Usually called by AS components so that they can get the updated values of
  // the store
  getLastUpdatedCells(): Array<ASCell> {
    return this.getState().lastUpdatedCells;
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
        const cell = getCell(this.getState(), ASIndex.fromNaked({
          col: currentColumn,
          row: currentRow,
        }));

        return cell == null ? '' : '' + U.Render.showValue(cell.value);
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
  // XXX this should definitely not need to happen according to Immutable's API
  // but otherwise following line throws an invalid keyPath error.
  let data_ = data.setIn(['allCells', sheetId], Map());
  data_ = data_.setIn(['allCells', sheetId, col, row], cell);
  return setErrors(data_, cell);
}

// Remove a cell at an ASIndex
function removeIndex(data: CellStoreData, loc: ASIndex): CellStoreData {
  let data_ = data;
  const emptyCell = ASCell.emptyCellAt(loc);
  if (locationExists(data_, loc)) {
    data_ = data.deleteIn(['allCells', loc.sheetId, loc.col, loc.row]);
  }

  data_ = data_.update('lastUpdatedCells', cells => cells.push(emptyCell));
  return unsetErrors(data_, emptyCell);
}


// Replace cells with empty ones
function removeCells(
  data: CellStoreData,
  cells: Array<ASCell>
): CellStoreData {
  let data_ = data;
  cells.forEach((cell) => {
    data_ = removeIndex(data, cell.location);
  });
  return data_;
}

// Function to update cell related objects in store. Caller's responsibility to
// clear lastUpdatedCells if necessary
function updateCells(
  data: CellStoreData,
  cells: Array<ASCell>
): CellStoreData {
  let removedCells = [];
  let data_ = data;
  cells.forEach(cell => {
    if (!cell.isEmpty()) {
      data_ = setCell(data_, cell);
      data_ = data_.update('lastUpdatedCells', cells => cells.push(cell));
    } else {
      // filter out all the blank cells passed back from the store
      removedCells.push(cell);
    }
  });
  return removeCells(data_, removedCells);
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
  Dispatcher.waitFor([ExpStore.dispatcherIndex]);
  return {
    language: ExpStore.getLanguage(),
    expression: ExpStore.getExpression(),
  };
}

// A lot of things listen to this store, eventemitter think's there's a memory
// leak
// TODO(joel) - is this an eventemitter? how does this work?
// ASCellStore.setMaxListeners(100);
//
export default new ASCellStore(Dispatcher);
