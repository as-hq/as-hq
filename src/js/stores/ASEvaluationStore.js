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
        ASEvaluationStore.updateCells(action.newCells);
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
  })


const ASEvaluationStore = assign({}, BaseStore, {

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

  // Requires that a range having row2 implies a range has col2, and vice versa
  setActiveSelection(sel, xp) {
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
  updateCells(cells) {
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
    _data.lastActiveFocus = _data.activeFocus;
    _data.activeFocus = elem;
  },

  getFocus() { return _data.activeFocus; },

  toggleFocusF2() {
    console.log("last focus: ", _data.activeFocus);
    let temp = _data.activeFocus;
    if (_data.activeFocus === 'grid' && _data.lastActiveFocus === 'textbox')
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

  moveToDataBoundary(direction, isShifted) {
    let sel = _data.activeSelection.range,
        {tl, br} = sel,
        origin = _data.activeSelection.origin,
        startExists,
        startRow, startCol,
        isOrientedH = origin.col === tl.col,
        isOrientedV = origin.row === tl.row,
        hExtremum = isOrientedH ? br.col : tl.col,
        vExtremum = isOrientedV ? br.row : tl.row,
        win = _data.viewingWindow.range,
        result;
    // console.log("\n\nin data bundary func\n\n", sel);
    // console.log("\n\nhextremum\n\n", hExtremum);
    switch(direction) {
      case "Right":
        startCol = this.locationExists(hExtremum+1, tl.row) ? hExtremum : hExtremum+1;
        startExists = this.locationExists(startCol, tl.row);
        for (var col = startCol; col < startCol + Constants.LARGE_SEARCH_BOUND; col++){
          let thisExists = this.locationExists(col, origin.row);
          if (Util.xor(startExists, thisExists)) {
            let idx = thisExists ? {col: col, row: tl.row} : {col: col-1, row: tl.row};
            if (isShifted) {
              let resultCol = isOrientedH ? tl.col : idx.col;
              let resultCol2 = isOrientedH ? idx.col : br.col;
              result = { tl: {row: tl.row, col: resultCol},
                         br: {row: br.row, col: resultCol2} };
            } else {
              result = {tl: idx, br: idx};
            }
            break;
          }
        }
        result = result || { tl: {row: isShifted ? tl.row : origin.row,
                                  col: isShifted ? origin.col : win.br.col},
                             br: {row: isShifted ? br.row : origin.row,
                                  col: win.br.col} };
        break;
      case "Down":
        startRow = this.locationExists(tl.col, vExtremum+1) ? vExtremum : vExtremum+1;
        startExists = this.locationExists(tl.col, startRow);
        for (var row = startRow; row < startRow + Constants.LARGE_SEARCH_BOUND; row++){
          let thisExists = this.locationExists(origin.col, row);
          if (Util.xor(startExists, thisExists)) {
            let idx = thisExists ? {col: tl.col, row: row} : {col: tl.col, row: row-1};
            if (isShifted) {
              let resultRow = isOrientedV ? tl.row : idx.row;
              let resultRow2 = isOrientedV ? idx.row : br.row;
              result = { tl: {row: resultRow, col: tl.col}, br: {row: resultRow2, col: br.col} };
            } else {
              result = {tl: idx, br: idx};
            }
            break;
          }
        }
        result = result || { tl: {row: isShifted ? origin.row : win.br.row,
                                  col: isShifted ? tl.col : origin.col},
                             br: {row: isShifted ? br.row : win.br.row,
                                  col: isShifted ? br.col : origin.col} };
        break;
      case "Left":
        startCol = this.locationExists(hExtremum-1, tl.row) ? hExtremum : hExtremum-1;
        startExists = this.locationExists(startCol, tl.row);
        for (var col = startCol; col > 1; col--) {
          let thisExists = this.locationExists(col, origin.row);
          if (Util.xor(startExists, thisExists)) {
            let idx = thisExists ? {col: col, row: tl.row} : {col: col+1, row: tl.row};
            if (isShifted) {
              let resultCol = isOrientedH ? tl.col : idx.col;
              let resultCol2 = isOrientedH ? idx.col : br.col;
              result = { tl: {row: tl.row, col: resultCol}, br: {row: br.row, col: resultCol2} };
            } else {
              result = {tl: idx, br: idx};
            }
            break;
          }
        }
        result = result || { tl: {row: tl.row, col: 1},
                             br: {row: isShifted ? br.row : tl.row,
                                  col: isShifted ? (isOrientedH ? tl.col : br.col) : 1} };
        break;
      case "Up":
        startRow = this.locationExists(tl.col, vExtremum-1) ? vExtremum : vExtremum-1;
        startExists = this.locationExists(tl.col, startRow);
        for (var row = startRow; row > 1; row--) {
          let thisExists = this.locationExists(origin.col, row);
          if (Util.xor(startExists, thisExists)) {
            let idx = thisExists ? {col: tl.col, row: row} : {col: tl.col, row: row+1};
            if (isShifted) {
              let resultRow = isOrientedV ? tl.row : idx.row;
              let resultRow2 = isOrientedV ? idx.row : br.row;
              result = { tl: {row: resultRow, col: tl.col}, br: {row: resultRow2, col: br.col} };
            } else {
              result = {tl: idx, br: idx};
            }
            break;
          }
        }
        result = result || { tl: {row: 1, col: tl.col},
                             br: {row: isShifted ? (isOrientedV ? tl.row : br.row) : 1,
                                  col: isShifted ? br.col : tl.col} };
        break;
    }
    return Util.orientRange(result);
  },

  // TODO actually get the data boundaries by iterating, or something
  // (but as long as we're using LARGE_SEARCH_BOUND, this area is an upper bound)
  getDataBounds() {
    return { tl: {col: 1, row: 1},
             br: {col: Constants.LARGE_SEARCH_BOUND,
                  row: Constants.LARGE_SEARCH_BOUND} };
  }


});


export default ASEvaluationStore;
