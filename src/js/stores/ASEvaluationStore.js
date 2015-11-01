import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import ReplStore from  './ASReplStore';
import assign from 'object-assign';
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

let _data = {
  userId: "TEST_USER_ID",
  allCells: {},
  lastUpdatedCells: [],
  suppressErrors: false, 
  xscroll: 0,
  yscroll: 0,
  openSheets: [],
  currentSheet: {
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
  partialSelections: [],
  activeCell: null,
  clipboard: {
    range: null,
    isCut: false
  },
  externalError: null,
  viewingWindow: null
};

const ASEvaluationStore = assign({}, BaseStore, {

  /* This function describes the actions of the ASEvaluationStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register(function (action) {
      switch (action.type) {
        case Constants.ActionTypes.FIND_INCREMENTED:
          break;
        case Constants.ActionTypes.FIND_DECREMENTED:
          break;
        case Constants.ActionTypes.GOT_FIND:
          // do nothing here on find; that's in the find store
          break;
        case Constants.ActionTypes.CELL_CHANGED:
          break;
        case Constants.ActionTypes.RANGE_CHANGED:
          break;
        /*
          On an UNDO/REDO/UPDATE_CELLS, update the viewing window in the store based on the commit and
          send a change event to spreadsheet, which will rerender
        */
        case Constants.ActionTypes.GOT_UNDO:
          console.log("action undo");
          _data.lastUpdatedCells = [];
          ASEvaluationStore.removeCells(action.commit.after);
          ASEvaluationStore.updateCells(action.commit.before);
          ASEvaluationStore.emitChange();
          break;
        case Constants.ActionTypes.GOT_REDO:
          _data.lastUpdatedCells = [];
          ASEvaluationStore.removeCells(action.commit.before);
          ASEvaluationStore.updateCells(action.commit.after);
          ASEvaluationStore.emitChange();
          break;
        case Constants.ActionTypes.GOT_UPDATED_CELLS:
          _data.lastUpdatedCells = [];
          ASEvaluationStore.updateCells(action.updatedCells);
          // console.log("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
          ASEvaluationStore.emitChange();
          break;
        /*
          This action is sent to Dispatcher by the ASSpreadsheet action creator on a scroll event
          It gets previous scroll state from the store and then uses the API to send a "get cells" message to server
        */
        case Constants.ActionTypes.SCROLLED:
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
        case Constants.ActionTypes.FETCHED_CELLS:
          _data.lastUpdatedCells = [];
          ASEvaluationStore.updateCells(action.newCells, true);
          // console.log("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
          ASEvaluationStore.emitChange();
          break;
        /*
          The server has cleared everything from the DB
          Need to delete the store
          Called from Dispatcher, fired by API response from server
        */
        case Constants.ActionTypes.CLEARED:
          _data.lastUpdatedCells = [];
          let cellsToRemove = [];
          for (var s in _data.allCells){
            for (var c in _data.allCells[s]){
              for (var r in _data.allCells[s][c]){
                cellsToRemove.push(_data.allCells[s][c][r]);
              }
            }
          }

          // remove possibly null cells
          cellsToRemove = cellsToRemove.filter((cell) => !!cell);

          ASEvaluationStore.removeCells(cellsToRemove);
          _data.allCells = {};
          // console.log("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
          ASEvaluationStore.emitChange();
          break;

        case Constants.ActionTypes.GOT_SELECTION:
          ASEvaluationStore.setActiveSelection(TC.asSelectionToSimple(action.newSelection), "");
          ASEvaluationStore.emitChange();
          break;
        case Constants.ActionTypes.DELETED_LOCS:
          ASEvaluationStore.removeLocs(action.locs);
          ASEvaluationStore.emitChange();
          break;
        case Constants.ActionTypes.GOT_FAILURE:
          ASEvaluationStore.setExternalError(action.errorMsg.result.failDesc);
          if (action.errorMsg.action === "EvaluateRepl"){
            ReplStore.advanceLine();
          }
          ASEvaluationStore.emitChange();
          break;
        case Constants.ActionTypes.RECEIEVED_SHEET:
          // TODO
          break;
        case Constants.ActionTypes.RECEIVED_WORKBOOK:
          // TODO
          break;
        }
    }),

  /**************************************************************************************************************************/
  /* getter and setter methods */
  getUserId() {
    return _data.userId;
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
    _data.currentSheet = {sheetId: sheetId, sheetName: "", sheetPermissions: {
      tag: 'Blacklist',
      contents: []
    }};
  },

  setActiveSelection(sel, xp) {
    console.log(sel);
    let origin = sel.origin;
    _data.activeSelection = sel;
    _data.activeCell = this.getCell(origin.col, origin.row) || TC.makeEmptyCell();
    var activeCellDependencies = Util.parseDependencies(xp);
    let c = sel.origin.col,
        r = sel.origin.row,
        listDep = this.getParentList(c, r);
    if (listDep !== null) {
      activeCellDependencies.push(listDep);
    }
    this.setActiveCellDependencies(activeCellDependencies);
  },

  getParentList(c,r){
    let thisExists = this.locationExists(c, r),
        ctags = thisExists ? this.getCell(c,r).cellTags : null;
    if (thisExists && ctags) {
      for (var i = 0; i < ctags.length; ++i) {
        if (ctags[i].hasOwnProperty('listKey')){
          let listHead = Util.listKeyToListHead(ctags[i].listKey),
              listDimensions = Util.listKeyToListDimensions(ctags[i].listKey);
          return {
            tl: {row: listHead.snd,
                 col: listHead.fst} ,
            br: {row: listHead.snd + listDimensions.fst - 1,
                 col: listHead.fst + listDimensions.snd - 1}
          }
        }
      }
      return null;
    } else return null;
  },


  getActiveSelection() {
    return _data.activeSelection;
  },

  getActiveCell() {
    return _data.activeCell;
  },
  getActiveCellDependencies() {
    return(_data.activeCell.cellExpression.dependencies);
  },
  setClipboard(rng, isCut) {
    _data.clipboard.area = rng;
    _data.clipboard.isCut = isCut;
    Render.setMode(rng === null ? null : (isCut ? 'cut' : 'copy'));
  },
  getClipboard() {
    return _data.clipboard;
  },
  setScroll(x, y){
    _data.xscroll = x;
    _data.yscroll = y;
  },
  getScroll() {
    return {x: _data.xscroll, y: _data.yscroll};
  },
  /* Usually called by AS components so that they can get the updated values of the store */
  getLastUpdatedCells(){
    return _data.lastUpdatedCells;
  },
  resetLastUpdatedCells() {
    _data.lastUpdatedCells = [];
  },
  addTag(tag, col, row) {
    let sheetId = _data.currentSheet.sheetId;
    if (this.locationExists(col, row, sheetId)){
      _data.allCells[sheetId][col][row].cellTags.push(tag);
      API.addTags([tag], TC.makeASIndex(sheetId, col, row));
    }
  },
  setExternalError(err) {
    _data.externalError = err;
  },
  getExternalError(){
    return _data.externalError;
  },


  /**************************************************************************************************************************/
  /* Copy paste helpers */

   // Converts a range to a row major list of lists of values,
   getRowMajorCellValues(rng){
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
  updateCells(cells, suppressErrors) {
    if (typeof(suppressErrors) == "undefined") suppressErrors = false; 

    _data.suppressErrors = suppressErrors; 

    let removedCells = [];
    for (var key in cells){
      let c = cells[key],
          xpString = c.cellExpression.expression;
      if (xpString != "") {
        this.setCell(c);
        _data.lastUpdatedCells.push(c);

      } else {
        removedCells.push(c); // filter out all the blank cells passed back from the store
      }
    }
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

  /* Replace cells with empty ones. Caller's responsibility to clear lastUpdatedCells if necessary */
  removeCells(cells) {
    for (var key in cells){
      let c = cells[key],
          emptyCell = TC.makeEmptyCell(c.cellLocation);
      this.removeIndex(c.cellLocation);
      _data.lastUpdatedCells.push(emptyCell);
    }
  },

  /* Remove a cell at an ASIndex */
  removeIndex(loc) {
    if (this.locationExists(loc.index.col, loc.index.row, loc.sheetId)) {
      _data.allCells[loc.sheetId][loc.index.col][loc.index.row] = null;
    }
  },

  /* Remove cells at ASRanges or ASIndices */
  removeLocs(locs) {
    let dlocs = Util.decomposeASLocations(locs);
    dlocs.forEach((l) => this.removeIndex(l), this);
  },

  clearSheetCacheById(sheetId) {
    _data.allCells[sheetId] = null;
  },

  setActiveCellDependencies(deps) {
    _data.activeCell.cellExpression.dependencies = deps;
    Render.setDependencies(deps);
  },

// @optional mySheetId
  locationExists(col, row, mySheetId) {
    let sheetId = mySheetId || _data.currentSheet.sheetId;
    return (_data.allCells[sheetId] && _data.allCells[sheetId][col] && _data.allCells[sheetId][col][row]);
  },

  /**************************************************************************************************************************/
  /* Focus */

  setFocus(elem) {
    console.log("\n\nFOCUS\n\n", elem);
    console.log("\nACTIVE SEL AFTER FOCUS\n", _data.activeSelection.origin);
    _data.lastActiveFocus = _data.activeFocus;
    _data.activeFocus = elem;
  },

  getFocus() { return _data.activeFocus; },

  toggleFocusF2(textboxIsVisible) {
    console.log("last focus: ", _data.activeFocus);
    let temp = _data.activeFocus;
    if (_data.activeFocus === 'grid' && _data.lastActiveFocus === 'grid' && textboxIsVisible)
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
    console.log("new focus: ", _data.activeFocus);
  },

  /**************************************************************************************************************************/
  /* Updating expression when user clicks on a cell */

// @optional mySheetId
  getCell(col,row,mySheetId){
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
      default: throw "Invalid direction passed in tp getDataBoundary()"; break;
    }

    let c = start.col, r = start.row;
    while (c >= 1 && r >= 1 && c <= Constants.numCols && r <= Constants.numRows) {
      c += dc;
      r += dr;
      if (this.locationExists(c, r)
       && !(this.locationExists(c + dc, r + dr) && this.locationExists(c - dc, r - dr))) {
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
  getDataBoundSelection(direction) {
    let rng = _data.activeSelection.range,
        {tl, br} = rng,
        origin = _data.activeSelection.origin;

    let startLoc = { row: origin.row, col: origin.col };
    if (direction == "Up" || direction == "Down") {
      if (origin.row > tl.row)
        startLoc.row = tl.row;
      else
        startLoc.row = br.row;
    } else if (direction == "Right" || direction == "Left") {
      if (origin.col < tl.col)
        startLoc.col = tl.col;
      else
        startLoc.col = br.col;
    } else {
      throw "Invalid direction passed in";
    }

    let bound = this.getDataBoundary(startLoc, direction);

    let newTl = tl;
    let newBr = br;

    if (direction == "Up" || direction == "Down") {
      if (origin.row > tl.row)
        newTl.row = bound.row;
      else
        newBr.row = bound.row;
    } else if (direction == "Right" || direction == "Left") {
      if (origin.col < tl.col)
        newTl.col = bound.col;
      else
        newBr.col = bound.col;
    }

    return { range: {tl: newTl, br: newBr}, origin: origin };
  },

  // TODO actually get the data boundaries by iterating, or something
  // (but as long as we're using LARGE_SEARCH_BOUND, this area is an upper bound)
  getDataBounds() {
    return { tl: {col: 1, row: 1},
             br: {col: Constants.LARGE_SEARCH_BOUND,
                  row: Constants.LARGE_SEARCH_BOUND} };
  },

  shouldSuppressErrors() {
    return _data.suppressErrors; 
  }


});


export default ASEvaluationStore;
