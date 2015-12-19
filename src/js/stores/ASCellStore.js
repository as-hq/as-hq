/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ASRange,
  ASSheet,
  ASCell,
  ASLanguage,
  ASSelection
} from '../types/Eval';

import type {
  ASCellGrid,
  ASFocusType
} from '../types/State';

import type {
  ASUserId
} from '../types/User';

import _ from 'lodash';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';
import API from '../actions/ASApiActionCreators';

import U from '../AS/Util';

import Render from '../AS/Renderers';
import SheetStateStore from './ASSheetStateStore.js';
import SelectionStore from './ASSelectionStore.js';
/*
Private variable keeping track of a viewing window (cached) of cells. Stores:
  1) Sheet name
  2) All cells in viewing window, indexed [sheet][col][row]
  3) Cells that were last updated by an eval or change event (so that components can easily access the update from the store)
  4) Scroll position
*/

type CellStoreData = {
  allCells: ASCellGrid;
  lastUpdatedCells: Array<ASCell>;
};

let _data: CellStoreData = {
  allCells: {},
  lastUpdatedCells: []
};

const ASCellStore = Object.assign({}, BaseStore, {

  /* This function describes the actions of the ASCellStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register((action) => {
    logDebug('Store received action', action);
    switch (action._type) {
      /*
        On an UNDO/REDO/UPDATE_CELLS, update the viewing window in the store based on the commit and
        send a change event to spreadsheet, which will rerender
      */
      case 'GOT_UNDO':
        logDebug("action undo");
        _data.lastUpdatedCells = [];
        ASCellStore.removeCells(action.commit.cellDiff.afterCells);
        ASCellStore.updateCells(action.commit.cellDiff.beforeCells);
        ASCellStore.emitChange();
        break;
      case 'GOT_REDO':
        _data.lastUpdatedCells = [];
        ASCellStore.removeCells(action.commit.cellDiff.beforeCells);
        ASCellStore.updateCells(action.commit.cellDiff.afterCells);
        ASCellStore.emitChange();
        break;
      case 'GOT_UPDATED_CELLS':
        _data.lastUpdatedCells = [];
        ASCellStore.updateCells(action.updatedCells);
        // logDebug("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
        ASCellStore.emitChange();
        break;
      /*
        The cells have been fetched from the server for a get request (for example, when scrolling)
        We now need to update the store based on these new values
        Called from Dispatcher, fired by API response from server
      */
      case 'FETCHED_CELLS':
        _data.lastUpdatedCells = [];
        ASCellStore.updateCells(action.newCells);
        // logDebug("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
        ASCellStore.emitChange();
        break;
      /*
        The server has cleared everything from the DB
        Need to delete the store
        Called from Dispatcher, fired by API response from server
      */
      case 'CLEARED':
        _data.lastUpdatedCells = [];
        var cellsToRemove = [];
        for (var s in _data.allCells) {
          _data.allCells[s].forEach((colArray) => {
            colArray.forEach((cell) => {
              cellsToRemove.push(cell);
            });
          });
      }

        // remove possibly null cells
        cellsToRemove = cellsToRemove.filter((cell) => !!cell);

        ASCellStore.removeCells(cellsToRemove);
        _data.allCells = {};
        // logDebug("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
        ASCellStore.emitChange();
        break;

      case 'CLEARED_SHEET':
        _data.lastUpdatedCells = [];

        if (_data.allCells[action.sheetId]) {
          let cr = [];
          _data.allCells[action.sheetId].forEach((colArray) => {
            colArray.forEach((cell) => {
              cr.push(cell);
            });
          });

          // remove possibly null cells
          cr = cr.filter((cell) => !!cell);

          ASCellStore.removeCells(cr);
          _data.allCells[action.sheetId] = [];
          // logDebug("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
          ASCellStore.emitChange();
        }

        break;
      case 'DELETED_LOCS':
        _data.lastUpdatedCells = [];
        let locs = U.Conversion.rangeToASIndices(action.deletedRange.range);
        ASCellStore.removeIndices(locs);
        ASCellStore.updateCells(action.updatedCells);
        ASCellStore.emitChange();
        break;
      case 'GOT_IMPORT':
        _data.lastUpdatedCells = [];
        let sheetId = action.newCells[0].cellLocation.sheetId; // assumes all imported cells are within the same sheet, which should be true.
        // first, remove cells in current sheet
        var cellsToRemove = [];
        _data.allCells[sheetId].forEach((colArray) => {
          colArray.forEach((cell) => {
            cellsToRemove.push(cell);
          });
        });
        cellsToRemove = cellsToRemove.filter((cell) => !!cell); // remove nulls
        ASCellStore.removeCells(cellsToRemove);
        _data.allCells[sheetId] = [];
        // then, update with the imported cells
        ASCellStore.updateCells(action.newCells);
        ASCellStore.emitChange();
        break;
    }
  }),

  /**************************************************************************************************************************/
  /* getter and setter methods */

  getActiveCell() {
    return SelectionStore.withActiveSelection(({origin}) => {
      return this.getCell(origin);
    });
  },

  setActiveCellDependencies(deps) {
    let cell = this.getActiveCell();
    Render.setDependencies(deps);
    if (!cell || !cell.cellExpression) {
      return;
    }
    cell.cellExpression.dependencies = deps;
  },

  getActiveCellDependencies() {
    let cell = this.getActiveCell();
    if (cell) {
      return (cell.cellExpression.dependencies);
    } else {
      return null;
    }
  },

  getParentList(loc: NakedIndex) {
    let cell = this.getCell(loc);
    if (cell) {
      let cProps = cell.cellProps;
      if (cProps) {
        let listKeyTag =
          cProps.filter((cProp) => cProp.hasOwnProperty('listKey'))[0];
        if (listKeyTag && listKeyTag.listKey) { // listKey flow hack
          let {listKey} = listKeyTag;
          let listHead = U.Conversion.listKeyToListHead(listKey);
          let listDimensions = U.Conversion.listKeyToListDimensions(listKey);
          return {
            tl: {row: listHead.snd,
                 col: listHead.fst} ,
            br: {row: listHead.snd + listDimensions.fst - 1,
                 col: listHead.fst + listDimensions.snd - 1}
          }
        }
      }
    }

    return null;
  },

  /* Usually called by AS components so that they can get the updated values of the store */
  getLastUpdatedCells(): Array<ASCell> {
    return _data.lastUpdatedCells;
  },

  resetLastUpdatedCells() {
    _data.lastUpdatedCells = [];
  },

  /**************************************************************************************************************************/
  /* Copy paste helpers */

   // Converts a range to a row major list of lists of values,
   getRowMajorCellValues(rng) {
     if (U.Location.isIndex(rng)) {
      let cell = this.getCell(rng.tl);
      return [[cell ? cell.cellValue.contents : '']];
     } else {
      let {tl, br} = rng,
          height = br.row - tl.row + 1,
          length = br.col - tl.col + 1,
          self = this,
          rowMajorValues = U.Array.make2DArrayOf("", height, length);
      for (let i = 0; i < height; ++i) {
        let currentRow = tl.row + i;
        rowMajorValues[i] = rowMajorValues[i].map(function(value, index) {
            let currentColumn = tl.col + index,
                cell = self.getCell({col: currentColumn, row: currentRow});
            return cell ? cell.cellValue.contents : "";
        });
      }
      return rowMajorValues;
     }
   },


  /**************************************************************************************************************************/
  /*
    Update methods to allCells and lastUpdatedCells.
    A cell in this class and stored in _data has the format from CellConverter, returned from eval
  */

  addCell(cell, sheetid, col, row) {
    if (!_data.allCells[sheetid])
      _data.allCells[sheetid] = [];
    if (!_data.allCells[sheetid][col])
      _data.allCells[sheetid][col] = [];
    _data.allCells[sheetid][col][row] = cell;
  },

  /* Function to update cell related objects in store. Caller's responsibility to clear lastUpdatedCells if necessary */
  updateCells(cells) {
    let removedCells = [];
    cells.forEach((c) => {
      if (!U.Cell.isEmptyCell(c)) {
        this.setCell(c);
        _data.lastUpdatedCells.push(c);
      } else {
        removedCells.push(c); // filter out all the blank cells passed back from the store
      }
    }, this);
    this.removeCells(removedCells);
  },

  /* Set an ASCell */
  setCell(c) {
    let {col, row} = c.cellLocation.index,
        sheetId = c.cellLocation.sheetId;
    if (!_data.allCells[sheetId]) _data.allCells[sheetId] = [];
    if (!_data.allCells[sheetId][col]) _data.allCells[sheetId][col] = [];
    _data.allCells[sheetId][col][row] = c;
  },

  // Replace cells with empty ones
  removeCells(cells: Array<ASCell>) {
    cells.forEach((cell) => {
      this.removeIndex(cell.cellLocation);
    });
  },

  // Remove a cell at an ASIndex
  removeIndex(loc: ASIndex) {
    let sheetId = loc.sheetId,
        emptyCell = U.Conversion.makeEmptyCell(loc);
    if (this.locationExists(loc.index.col, loc.index.row, sheetId)) {
      delete _data.allCells[sheetId][loc.index.col][loc.index.row];
    }

    _data.lastUpdatedCells.push(emptyCell);
  },

  // Remove cells at a list of ASIndices.
  removeIndices(locs) {
    locs.forEach((l) => this.removeIndex(l), this);
  },

  clearSheetCacheById(sheetId) {
    _data.allCells[sheetId] = null;
  },

  // @optional mySheetId
  locationExists(col, row, sheetId) {
    return !!(_data.allCells[sheetId]
      && _data.allCells[sheetId][col]
      && _data.allCells[sheetId][col][row]);
  },

  isNonBlankCell(col, row, mySheetId?: string) {
    let sheetId = mySheetId || SheetStateStore.getCurrentSheet().sheetId;
    return this.locationExists(col, row, sheetId) && _data.allCells[sheetId][col][row].cellExpression.expression != "";
  },

  // @optional mySheetId
  getCell({col, row}: NakedIndex): ?ASCell {
    let sheetId = SheetStateStore.getCurrentSheet().sheetId;
    if (this.locationExists(col, row, sheetId))
      return _data.allCells[sheetId][col][row];
    else {
      return null;
    }
  },

  getCells({tl, br}: NakedRange): Array<Array<?ASCell>> {
    let sheetId = SheetStateStore.getCurrentSheet().sheetId;
    return _.range(tl.col, br.col).map((c) => {
      return _.range(tl.row, br.row).map((r) => {
        return (this.locationExists(c, r, sheetId)) ?
                _data.allCells[sheetId][c][r] :
                null;
      });
    });
  },

  getValues(rng: NakedRange): Array<Array<string | number>> {
    return this.getCells(rng).map((cs) => cs.map(this.cellToJSVal));
  },

  cellToJSVal(c: ASCell): string | number {
    switch (c.cellValue.tag) {
      case "ValueI":
      case "ValueD":
      case "ValueS":
        return c.cellValue.contents;
      default:
        return null;
    };
  }
});


export default ASCellStore;
