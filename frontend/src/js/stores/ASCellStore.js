/* @flow */

import type {
  ASClientError
} from '../types/Errors';

import type {
  ASLocation,
  ASSheet,
  ASLanguage,
  RangeDescriptor
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

import ASCell from '../classes/ASCell';
import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

import Render from '../AS/Renderers';
import SheetStateStore from './ASSheetStateStore.js';
import SelectionStore from './ASSelectionStore.js';
import DescriptorStore from './ASRangeDescriptorStore.js';
/*
Private variable keeping track of a viewing window (cached) of cells. Stores:
  1) Sheet name
  2) All cells in viewing window, indexed [sheet][col][row]
  3) Cells that were last updated by an eval or change event (so that components can easily access the update from the store)
  4) Scroll position
*/

type CellStoreData = {
  allCells: ASCellGrid;
  allErrors: Array<ASClientError>;
  lastUpdatedCells: Array<ASCell>;
};

let _data: CellStoreData = {
  allCells: {},
  allErrors: [],
  lastUpdatedCells: []
};

const ASCellStore = Object.assign({}, BaseStore, {

  /* This function describes the actions of the ASCellStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register((action) => {
    logDebug('Cell store received action', action);
    switch (action._type) {
      /*
        The cells have been fetched from the server for a get request (for example, when scrolling)
        We now need to update the store based on these new values
        Called from Dispatcher, fired by API response from server
      */
      case 'GOT_UPDATED_CELLS':
        // The expanding types that we put into the cells depends on the updated range descriptors
        Dispatcher.waitFor([DescriptorStore.dispatcherIndex]);

        _data.lastUpdatedCells = [];
        ASCellStore.removeLocations(action.oldLocs);

        // NOTE: removed the following line because expanding types are set in the constructor of ASCell now
        // let newCellsWithExpandingTypes = ASCellStore._addExpandingTypesToCells(action.newCells);
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
    }
  }),

  /**************************************************************************************************************************/
  /* getter and setter methods */

  getActiveCell() {
    return SelectionStore.withActiveSelection(({origin}) => {
      return ASCellStore.getCell(origin);
    });
  },

  setActiveCellDependencies(deps) {
    let cell = ASCellStore.getActiveCell();
    Render.setDependencies(deps);
    if (!cell || !cell.cellExpression) {
      return;
    }
    cell.cellExpression.dependencies = deps;
  },

  getActiveCellDependencies() {
    let cell = ASCellStore.getActiveCell();
    if (cell) {
      return (cell.expression.dependencies);
    } else {
      return null;
    }
  },

  getActiveCellDisplay(): ?string {
    let cell = ASCellStore.getActiveCell();
    if (!!cell) {
      // #needsrefactor mgao can you flow this file with classes
      return cell._display;
    } else {
      return null;
    }
  },

  getParentList(loc: ASIndex): ?ASRange {
    let cell = ASCellStore.getCell(loc);
    if (cell) {
      let cProps = cell.props;
      if (cProps) {
        let listKeyTag =
          cProps.filter((cProp) => cProp.hasOwnProperty('listKey'))[0];
        if (listKeyTag && listKeyTag.listKey) { // listKey flow hack
          let {listKey} = listKeyTag;
          let listHead = U.Conversion.listKeyToListHead(listKey);
          let listDimensions = U.Conversion.listKeyToListDimensions(listKey);

          return ASRange.fromNaked({
            tl: {row: listHead.snd,
                 col: listHead.fst} ,
            br: {row: listHead.snd + listDimensions.fst - 1,
                 col: listHead.fst + listDimensions.snd - 1}
          });
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

  // Converts a range to a row major list of lists of values, represented by their underlying strings.
  getRowMajorCellValues(rng: ASRange): Array<Array<string>> {
    let {tl, br} = rng,
        height = br.row - tl.row + 1,
        length = br.col - tl.col + 1,
        self = ASCellStore,
        rowMajorValues = U.Array.make2DArrayOf("", height, length); //
    for (let i = 0; i < height; ++i) {
      let currentRow = tl.row + i;
      rowMajorValues[i] = rowMajorValues[i].map(function(value, index) {
          let currentColumn = tl.col + index,
              cell = self.getCell(ASIndex.fromNaked({
                col: currentColumn, row: currentRow
              }));
          if (cell != null) {
            return String(U.Render.showValue(cell.value));
          } else {
            return "";
          };
      });
    }
    return rowMajorValues;
  },


  /**************************************************************************************************************************/
  /*
    Update methods to allCells and lastUpdatedCells.
    A cell in this class and stored in _data has the format from CellConverter, returned from eval
  */

  // xcxc: why exactly is this taking sheetid, col, and row when those are already going to be in the cell
  // TODO
  addCell(cell, sheetid, col, row) {
    if (!_data.allCells[sheetid])
      _data.allCells[sheetid] = [];
    if (!_data.allCells[sheetid][col])
      _data.allCells[sheetid][col] = [];
    _data.allCells[sheetid][col][row] = cell;
  },

  /* Function to update cell related objects in store. Caller's responsibility to clear lastUpdatedCells if necessary */
  updateCells(cells: Array<ASCell>) {
    let removedCells = [];
    cells.forEach((c) => {
      if (!c.isEmpty()) {
        ASCellStore.setCell(c);
        _data.lastUpdatedCells.push(c);
      } else {
        removedCells.push(c); // filter out all the blank cells passed back from the store
      }
    }, ASCellStore);
    ASCellStore.removeCells(removedCells);
  },

  getAllErrors(): Array<ASClientError> {
    return _data.allErrors;
  },

  setErrors(c: ASCell) {
    ASCellStore.unsetErrors(c);

    const { value: cv, expression: cxp, location: cl } = c;
    switch (cv.tag) {
      case 'ValueError':
        _data.allErrors.push({
          location: cl,
          language: cxp.language,
          msg: cv.errorMsg
        });
        break;
      default:
        return;
    }
  },

  unsetErrors(c: ASCell) {
    _data.allErrors = _data.allErrors.filter(
      ({ location }) => ! c.location.equals(location)
    );
  },

  /* Set an ASCell */
  setCell(c: ASCell) { //error here
    let {col, row, sheetId} = c.location;
    if (!_data.allCells[sheetId]) _data.allCells[sheetId] = [];
    if (!_data.allCells[sheetId][col]) _data.allCells[sheetId][col] = [];
    _data.allCells[sheetId][col][row] = c;

    ASCellStore.setErrors(c);
  },

  // Replace cells with empty ones
  removeCells(cells: Array<ASCell>) {
    cells.forEach((cell) => {
      ASCellStore.removeIndex(cell.location);
    });
  },

  // Remove a cell at an ASIndex
  removeIndex(loc: ASIndex) { //error here
    let emptyCell = ASCell.emptyCellAt(loc);
    if (ASCellStore.locationExists(loc)) {
      delete _data.allCells[loc.sheetId][loc.col][loc.row];
    }

    _data.lastUpdatedCells.push(emptyCell);

    ASCellStore.unsetErrors(emptyCell);
  },

  // Remove cells at a list of ASLocation's.
  removeLocations(locs: Array<ASLocation>) {
    U.Location.asLocsToASIndices(locs).forEach((i) => ASCellStore.removeIndex(i), ASCellStore);
  },

  clearSheetCacheById(sheetId) {
    _data.allCells[sheetId] = null;
  },

  // @optional mySheetId
  locationExists({col, row, sheetId}: ASIndex) {
    return !!(_data.allCells[sheetId]
      && _data.allCells[sheetId][col]
      && _data.allCells[sheetId][col][row]);
  },

  isNonBlankCell(idx: ASIndex) {
    const {col, row, sheetId} = idx;

    return ASCellStore.locationExists(idx) && _data.allCells[sheetId][col][row].expression.expression != "";
  },

  // @optional mySheetId
  getCell(loc: ASIndex): ?ASCell {
    if (ASCellStore.locationExists(loc))
      return _data.allCells[loc.sheetId][loc.col][loc.row];
    else {
      return null;
    }
  },

  getCells(rng: ASRange): Array<Array<?ASCell>> {
    return U.Array.map2d(rng.toIndices2d(), ASCellStore.getCell);
  },

  cellToJSVal(c: ASCell): ?(string|number) {
    switch (c.value.tag) {
      case "ValueI":
      case "ValueD":
      case "ValueS":
        return c.value.contents;
      default:
        return null;
    };
  }

/*
  // When we receive cell updates from the backend, we're given a list of *all* the cells that have changed,
  // including those that have gotten coupled/gotten decoupled. We need to figure out the expandingType's of these
  // cells, for rendering.
  _addExpandingTypesToCells(cs: Array<ASCell>): Array<ASCell> {
    let foos = cs.map((c) => ASCellStore._addExpandingTypeToCell(c));
    return foos;
  },

  _addExpandingTypeToCell(c: ASCell): ASCell {
    c.getExpandingType(); // TODO: xcxc: temporary hack until render directly calls getExpandingType

    return c;
  }, */
});


export default ASCellStore;
