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
import ExpStore from './ASExpStore';
import SheetStateStore from './ASSheetStateStore.js';
import SelectionStore from './ASSelectionStore.js';
import DescriptorStore from './ASRangeDescriptorStore.js';

/*
Private variable keeping track of a viewing window (cached) of cells. Stores:
  1) All cells, indexed [sheet][col][row]
  2) All errors
  3) Cells that were last updated by an eval or change event (so that
     components can easily access the update from the store)
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
        removeLocations(action.oldLocs);

        // NOTE: removed the following line because expanding types are set in the constructor of ASCell now
        // let newCellsWithExpandingTypes = ASCellStore._addExpandingTypesToCells(action.newCells);
        updateCells(action.newCells);
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

        removeCells(cellsToRemove);
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

          removeCells(cr);
          _data.allCells[action.sheetId] = [];
          // logDebug("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
          ASCellStore.emitChange();
        }

        break;

      case 'TEXTBOX_CHANGED':
      case 'GRID_KEY_PRESSED': {
        const {language} = waitForLanguageAndExpression();
        const deps = U.Parsing.parseDependencies(action.xpStr, language);
        setActiveCellDependencies(deps);
        break;
      }

      case 'PARTIAL_REF_CHANGE_WITH_GRID':
      case 'PARTIAL_REF_CHANGE_WITH_EDITOR':
      case 'PARTIAL_REF_CHANGE_WITH_TEXTBOX': {
        const {lang, expression} = waitForLanguageAndExpression();
        const deps: Array<ASRange> =
          U.Parsing.parseDependencies(expression, lang);
        setActiveCellDependencies(deps);
        break;
      }

      case 'GOT_SELECTION': {
        const {newSelection: {origin}} = action;
        ASCellStore._sideEffectingSetCellDependencies(origin);
        ASCellStore.emitChange();
        break;
      }

      case 'SET_ACTIVE_SELECTION': {
        const {selection: {origin}} = action;
        ASCellStore._sideEffectingSetCellDependencies(origin);
        ASCellStore.emitChange();
        break;
      }
    }
  }),

  // Side-effects by first waiting for ExpStore, then modifying the active
  // cell's dependencies
  _sideEffectingSetCellDependencies(origin: ASIndex) {
    const {language, expression} = waitForLanguageAndExpression();
    const activeCellDependencies: Array<ASRange> =
      U.Parsing.parseDependencies(expression, language);
    const listDep: ?ASRange = ASCellStore.getParentList(origin);
    if (listDep != null) {
      activeCellDependencies.push(listDep);
    }
    setActiveCellDependencies(activeCellDependencies);
  },

  /**************************************************************************************************************************/
  /* getter and setter methods */

  getActiveCell() {
    return SelectionStore.withActiveSelection(({origin}) => {
      return ASCellStore.getCell(origin);
    });
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

  getAllErrors(): Array<ASClientError> {
    return _data.allErrors;
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
  },

});

// A lot of things listen to this store, eventemitter think's there's a memory
// leak
ASCellStore.setMaxListeners(100);

function unsetErrors(c: ASCell) {
  _data.allErrors = _data.allErrors.filter(
    ({location}) => ! c.location.equals(location)
  );
}

function setErrors(c: ASCell) {
  unsetErrors(c);

  const {value: cv, expression: cxp, location: cl} = c;
  switch (cv.tag) {
    case 'ValueError':
      _data.allErrors.push({
        location: cl,
        language: cxp.language,
        msg: cv.errorMsg,
      });
      break;
    default:
      return;
  }
}

function setCell(c: ASCell) {
  const {col, row, sheetId} = c.location;
  if (!_data.allCells[sheetId]) _data.allCells[sheetId] = [];
  if (!_data.allCells[sheetId][col]) _data.allCells[sheetId][col] = [];
  _data.allCells[sheetId][col][row] = c;

  setErrors(c);
}

// Remove a cell at an ASIndex
function removeIndex(loc: ASIndex) {
  const emptyCell = ASCell.emptyCellAt(loc);
  if (ASCellStore.locationExists(loc)) {
    delete _data.allCells[loc.sheetId][loc.col][loc.row];
  }

  _data.lastUpdatedCells.push(emptyCell);
  unsetErrors(emptyCell);
}


// Replace cells with empty ones
function removeCells(cells: Array<ASCell>) {
  cells.forEach((cell) => {
    removeIndex(cell.location);
  });
}

// Function to update cell related objects in store. Caller's responsibility to
// clear lastUpdatedCells if necessary
function updateCells(cells: Array<ASCell>) {
  const removedCells = [];
  cells.forEach(cell => {
    if (!cell.isEmpty()) {
      setCell(cell);
      _data.lastUpdatedCells.push(cell);
    } else {
      removedCells.push(cell); // filter out all the blank cells passed back from the store
    }
  }, ASCellStore);
  removeCells(removedCells);
}
// Remove cells at a list of ASLocation's.
function removeLocations(locs: Array<ASLocation>) {
  U.Location.asLocsToASIndices(locs).forEach((i) => removeIndex(i), ASCellStore);
}

function setActiveCellDependencies(deps) {
  const cell = ASCellStore.getActiveCell();
  Render.setDependencies(deps);
  if (!cell || !cell.cellExpression) {
    return;
  }
  cell.cellExpression.dependencies = deps;
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

export default ASCellStore;
