/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ASRange,
  ASSheet,
  ASCell,
  ASLanguage
} from '../types/Eval';

import type {
  ASSelection,
  ASCellStore,
  ASFocusType
} from '../types/State';

import type {
  ASUserId
} from '../types/User';

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import ReplStore from  './ASReplStore';
import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';
import T from '../AS/Types';
import TC from '../AS/TypeConversions';
import Render from '../AS/Render';

/*
Private variable keeping track of a viewing window (cached) of cells. Stores:
  1) Sheet name
  2) All cells in viewing window, indexed [sheet][col][row]
  3) Cells that were last updated by an eval or change event (so that components can easily access the update from the store)
  4) Scroll position
*/

let _data: {
  userId: ASUserId;
  allCells: ASCellStore;
  lastUpdatedCells: Array<ASCell>;
  decoupleAttempt: boolean;
  suppressErrors: boolean;
  xscroll: number;
  yscroll: number;
  openSheets: Array<ASSheet>;
  currentSheet: ASSheet;
  activeSelection: ?ASSelection;
  activeFocus: ASFocusType;
  lastActiveFocus: ASFocusType;
  activeCell: ?ASCell;
  clipboard: {
    area: ?ASSelection;
    isCut: boolean;
  };
  externalError: ?string;
  viewingWindow: { range: NakedRange };
} = {
  userId: "TEST_USER_ID",
  allCells: {},
  lastUpdatedCells: [],
  decoupleAttempt: false,
  suppressErrors: false,
  xscroll: 0,
  yscroll: 0,
  openSheets: [],
  currentSheet: {
    tag: 'Sheet',
    sheetId: "INIT_SHEET_ID",
    sheetName: "Sheet1",
    sheetPermissions: {
      tag: 'Blacklist',
      contents: []
    }
  },
  activeSelection: null,
  activeFocus: 'grid',
  lastActiveFocus: 'textbox',
  activeCell: null,
  clipboard: {
    area: null,
    isCut: false
  },
  externalError: null,
  viewingWindow: {
    range: {
      tl: { col: 0, row: 0},
      br: { col: 100, row: 100}
    }
  }
};

const ASEvaluationStore = Object.assign({}, BaseStore, {

  /* This function describes the actions of the ASEvaluationStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register((action) => {
    logDebug('Store received action', action);

      switch (action._type) {
        case 'FIND_INCREMENTED':
          break;
        case 'FIND_DECREMENTED':
          break;
        case 'GOT_FIND':
          // do nothing here on find; that's in the find store
          break;
        /*
          On an UNDO/REDO/UPDATE_CELLS, update the viewing window in the store based on the commit and
          send a change event to spreadsheet, which will rerender
        */
        case 'GOT_UNDO':
          logDebug("action undo");
          _data.lastUpdatedCells = [];
          ASEvaluationStore.removeCells(action.commit.after);
          ASEvaluationStore.updateCells(action.commit.before);
          ASEvaluationStore.emitChange();
          break;
        case 'GOT_REDO':
          _data.lastUpdatedCells = [];
          ASEvaluationStore.removeCells(action.commit.before);
          ASEvaluationStore.updateCells(action.commit.after);
          ASEvaluationStore.emitChange();
          break;
        case 'GOT_UPDATED_CELLS':
          _data.lastUpdatedCells = [];
          ASEvaluationStore.updateCells(action.updatedCells);
          // logDebug("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
          ASEvaluationStore.emitChange();
          break;
        case 'EVAL_TRIED_TO_DECOUPLE':
          _data.decoupleAttempt = true;
          ASEvaluationStore.emitChange();
          break;
        /*
          This action is sent to Dispatcher by the ASSpreadsheet action creator on a scroll event
          It gets previous scroll state from the store and then uses the API to send a "get cells" message to server
        */
        case 'SCROLLED':
          let extendedRange = Util.extendRangeByCache(action.vWindow.range),
              extendedWindow = TC.rangeToASWindow(extendedRange);
          _data.viewingWindow = action.vWindow;
          API.updateViewingWindow(extendedWindow);
          break;
        /*
          The cells have been fetched from the server for a get request (for example, when scrolling)
          We now need to update the store based on these new values
          Called from Dispatcher, fired by API response from server
        */
        case 'FETCHED_CELLS':
          _data.lastUpdatedCells = [];
          _data.suppressErrors = true; // don't show errors when fetching cells. will get set to false at end of emitChange()
          ASEvaluationStore.updateCells(action.newCells);
          // logDebug("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
          ASEvaluationStore.emitChange();
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

          ASEvaluationStore.removeCells(cellsToRemove);
          _data.allCells = {};
          // logDebug("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
          ASEvaluationStore.emitChange();
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

            ASEvaluationStore.removeCells(cr);
            _data.allCells[action.sheetId] = [];
            // logDebug("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
            ASEvaluationStore.emitChange();
          }

          break;
        case 'GOT_SELECTION':
          ASEvaluationStore.setActiveSelection(TC.asSelectionToSimple(action.newSelection), "", null);
          ASEvaluationStore.emitChange();
          break;
        case 'DELETED_LOCS':
          _data.lastUpdatedCells = [];
          let locs = TC.rangeToASIndices(action.deletedRange.range);
          ASEvaluationStore.removeIndices(locs);
          ASEvaluationStore.updateCells(action.updatedCells);
          ASEvaluationStore.emitChange();
          break;
        case 'GOT_IMPORT':
          _data.lastUpdatedCells = [];
          _data.suppressErrors = true; // don't show errors when fetching cells. will get set to false at end of emitChange()
          let sheetId = action.newCells[0].cellLocation.sheetId; // assumes all imported cells are within the same sheet, which should be true.
          // first, remove cells in current sheet
          var cellsToRemove = [];
          _data.allCells[sheetId].forEach((colArray) => {
            colArray.forEach((cell) => {
              cellsToRemove.push(cell);
            });
          });
          cellsToRemove = cellsToRemove.filter((cell) => !!cell); // remove nulls
          ASEvaluationStore.removeCells(cellsToRemove);
          _data.allCells[sheetId] = [];
          // then, update with the imported cells
          ASEvaluationStore.updateCells(action.newCells);
          ASEvaluationStore.emitChange();
          break;
        case 'GOT_FAILURE':
          ASEvaluationStore.setExternalError(action.errorMsg);
          if (action.action === "EvaluateRepl") {
            ReplStore.advanceLine();
          }
          ASEvaluationStore.emitChange();
          break;
      }
    }),

  /**************************************************************************************************************************/
  /* getter and setter methods */
  getUserId() {
    return _data.userId;
  },
  getDecoupleAttempt() {
    return _data.decoupleAttempt;
  },
  setDecoupleAttempt(b) {
    _data.decoupleAttempt = b;
  },
  setUserId(id) {
    _data.userId = id;
  },
  getCurrentSheet() {
    return _data.currentSheet;
  },
  setCurrentSheet(sht) {
    _data.currentSheet = sht;
  },
  setCurrentSheetById(sheetId) {
    _data.currentSheet = {
      tag: 'Sheet',
      sheetId: sheetId,
      sheetName: "",
      sheetPermissions: {
        tag: 'Blacklist',
        contents: []
      }
    };
  },

  setActiveSelection(sel, xp, lang: ?ASLanguage) {
    Render.setSelection(sel);
    let origin = sel.origin;
    _data.activeSelection = sel;
    _data.activeCell = this.getCell(origin.col, origin.row) || TC.makeEmptyCell();
    var activeCellDependencies = Util.parseDependencies(xp, lang);
    let c = sel.origin.col,
        r = sel.origin.row,
        listDep = this.getParentList(c, r);
    if (listDep !== null) {
      activeCellDependencies.push(listDep);
    }
    this.setActiveCellDependencies(activeCellDependencies);
  },

  getParentList(c, r) {
    let cell = this.getCell(c, r);
    if (cell) {
      let cProps = cell.cellProps;
      if (cProps) {
        let listKeyTag =
          cProps.filter((cProp) => cProp.hasOwnProperty('listKey'))[0];
        if (listKeyTag && listKeyTag.listKey) { // listKey flow hack
          let {listKey} = listKeyTag;
          let listHead = Util.listKeyToListHead(listKey);
          let listDimensions = Util.listKeyToListDimensions(listKey);
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


  getActiveSelection() {
    return _data.activeSelection;
  },

  getActiveCell() {
    return _data.activeCell;
  },
  getActiveCellDependencies() {
    if (_data.activeCell) {
      return (_data.activeCell.cellExpression.dependencies);
    } else {
      return null;
    }
  },
  setClipboard(rng, isCut) {
    _data.clipboard.area = rng;
    _data.clipboard.isCut = isCut;
    Render.setMode(rng === null ? null : (isCut ? 'cut' : 'copy'));
  },
  getClipboard() {
    return _data.clipboard;
  },
  setScroll(x, y) {
    _data.xscroll = x;
    _data.yscroll = y;
  },
  getScroll() {
    return {x: _data.xscroll, y: _data.yscroll};
  },
  /* Usually called by AS components so that they can get the updated values of the store */
  getLastUpdatedCells() {
    return _data.lastUpdatedCells;
  },
  resetLastUpdatedCells() {
    _data.lastUpdatedCells = [];
  },

  // now handled entirely by backend
  // toggleTag(tag, rng) {
  //   let inds = TC.rangeToIndices(rng);
  //   inds.forEach((i) => this.toggleTagAtLoc(tag, i), this);
  // },
  // toggleTagAtLoc(tag, loc) {
  //   let {col, row} = loc;
  //   let sheetId = _data.currentSheet.sheetId;
  //   if (this.locationExists(col, row, sheetId)) {
  //     let ct = _data.allCells[sheetId][col][row].cellProps;
  //     let ind = -1;

  //     for (let i = 0; i < ct.length; i++) {
  //       if (ct[i].tag == tag.tag) {
  //         ind = i;
  //       }
  //     }

  //     // if not included, add it; if included, remove it
  //     (ind == -1) ? (ct.push(tag)) : (ct.splice(ind, 1));
  //   }
  // },

  setExternalError(err: ?string) {
    _data.externalError = err;
  },
  getExternalError() {
    return _data.externalError;
  },


  /**************************************************************************************************************************/
  /* Copy paste helpers */

   // Converts a range to a row major list of lists of values,
   getRowMajorCellValues(rng) {
     if (T.isIndex(rng)) {
      let cell = this.getCell(rng.tl.col, rng.tl.row);
      return [[cell ? cell.cellValue.contents : null]];
     } else {
      let {tl, br} = rng,
          height = br.row - tl.row + 1,
          length = br.col - tl.col + 1,
          self = this,
          rowMajorValues = Util.make2DArrayOf("", height, length);
      for (let i = 0; i < height; ++i) {
        let currentRow = tl.row + i;
        rowMajorValues[i] = rowMajorValues[i].map(function(value, index) {
            let currentColumn = tl.col + index,
                cell = self.getCell(currentColumn, currentRow);
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
      if (!Util.isEmptyCell(c)) {
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
        emptyCell = TC.makeEmptyCell(loc);
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

  setActiveCellDependencies(deps) {
    let cell = _data.activeCell;
    if (!cell || !cell.cellExpression) {
      return;
    }
    cell.cellExpression.dependencies = deps;
    Render.setDependencies(deps);
  },

// @optional mySheetId
  locationExists(col, row, mySheetId) {
    let sheetId = mySheetId || _data.currentSheet.sheetId;
    return !!(_data.allCells[sheetId]
      && _data.allCells[sheetId][col]
      && _data.allCells[sheetId][col][row]);
  },

  isNonBlankCell(col, row, mySheetId) { 
    let sheetId = mySheetId || _data.currentSheet.sheetId;
    return this.locationExists(col, row, mySheetId) && _data.allCells[sheetId][col][row].cellExpression.expression != "";
  },

  /**************************************************************************************************************************/
  /* Focus */

  setFocus(elem) {
    logDebug("FOCUS", elem);
    _data.lastActiveFocus = _data.activeFocus;
    _data.activeFocus = elem;
  },

  getFocus() { return _data.activeFocus; },

  toggleFocusF2() {
    logDebug("last focus: ", _data.activeFocus);
    let temp = _data.activeFocus;
    if (_data.activeFocus === 'grid' && _data.lastActiveFocus === 'grid')
      _data.activeFocus = 'textbox';
    else if (_data.activeFocus === 'grid' && _data.lastActiveFocus === 'textbox')
      _data.activeFocus = 'textbox';
    else if (_data.activeFocus === 'grid' && _data.lastActiveFocus === 'editor')
      _data.activeFocus = 'editor';
    else if (_data.activeFocus === 'textbox')
      _data.activeFocus = 'grid';
    else if (_data.activeFocus === 'editor')
      _data.activeFocus = 'grid';
    _data.lastActiveFocus = temp;
    logDebug("new focus: ", _data.activeFocus);
  },

  /**************************************************************************************************************************/
  /* Updating expression when user clicks on a cell */

// @optional mySheetId
  getCell(col,row,mySheetId) {
    let sheetId = mySheetId || _data.currentSheet.sheetId;
    if (this.locationExists(col, row, sheetId))
      return _data.allCells[sheetId][col][row];
    else {
      return null;
    }
  },

  getDataBoundary(start, direction) {
    let dr = 0, dc = 0;

    switch (direction) {
      case "Right": dc = 1; break;
      case "Left": dc = -1; break;
      case "Down": dr = 1; break;
      case "Up": dr = -1; break;
    }

    let c = start.col, r = start.row;
    while (c >= 1 && r >= 1 && c <= Constants.numCols && r <= Constants.numRows) {
      c += dc;
      r += dr;
      if (this.isNonBlankCell(c, r)
       && !(this.isNonBlankCell(c + dc, r + dr) && this.isNonBlankCell(c - dc, r - dr))) {
        break;
      }
    }

    if (c < 1) c = 1;
    if (r < 1) r = 1;
    if (c > Constants.numCols) c = Constants.numCols;
    if (r > Constants.numRows) r = Constants.numRows;

    return {col: c, row: r};
  },

  //This function returns what the new selection would be if you pressed ctrl+shift+right/up/left/down.
  //If shift is not held down,
  getDataBoundSelection(selection, direction) {
    let rng = selection.range,
        {tl, br} = rng,
        origin = selection.origin;

    let startLoc = { row: origin.row, col: origin.col };
    switch (direction) {
      case "Right": startLoc.col = (origin.col == tl.col) ? br.col : tl.col; break;
      case "Left": startLoc.col = (origin.col == br.col) ? tl.col : br.col; break;
      case "Up": startLoc.row = (origin.row == br.row) ? tl.row : br.row; break;
      case "Down": startLoc.row = (origin.row == tl.row) ? br.row : tl.row; break;
      default: throw "Invalid direction passed in";
    }

    let bound = this.getDataBoundary(startLoc, direction);

    // slight misnomers; these are the corners, but not necessarily top left or bottom right
    let newTl = {row: tl.row, col: tl.col};
    let newBr = {row: br.row, col: br.col};
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

    return { range: Util.orientRange({tl: newTl, br: newBr}), origin: origin };
  },

  // TODO actually get the data boundaries by iterating, or something
  // (but as long as we're using LARGE_SEARCH_BOUND, this area is an upper bound)
  getDataBounds() {
    return { tl: {col: 1, row: 1},
             br: {col: Constants.LARGE_SEARCH_BOUND,
                  row: Constants.LARGE_SEARCH_BOUND} };
  },

  getViewingWindow() {
    return _data.viewingWindow;
  },

  shouldSuppressErrors() {
    return _data.suppressErrors;
  },

  stopSuppressingErrors() {
    _data.suppressErrors = false;
  }
});


export default ASEvaluationStore;
