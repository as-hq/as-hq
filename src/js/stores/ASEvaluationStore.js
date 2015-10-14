import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import ReplStore from  './ASReplStore';
import assign from 'object-assign';
import API from '../actions/ASApiActionCreators';
import Converter from '../AS/Converter';
import Util from '../AS/Util';

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
  activeCell: null,
  clipboard: {
    range: null,
    isCut: false
  },
  externalError: null
};

/* This function describes the actions of the ASEvaluationStore upon recieving a message from Dispatcher */
dispatcherIndex: Dispatcher.register(function (action) {
    switch (action.type) {
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
        ASEvaluationStore.removeData(action.commit.after);
        ASEvaluationStore.updateData(action.commit.before);
        ASEvaluationStore.emitChange();
        break;
      case Constants.ActionTypes.GOT_REDO:
        _data.lastUpdatedCells = [];
        ASEvaluationStore.removeData(action.commit.before);
        ASEvaluationStore.updateData(action.commit.after);
        ASEvaluationStore.emitChange();
        break;
      case Constants.ActionTypes.GOT_UPDATED_CELLS:
        _data.lastUpdatedCells = [];
        ASEvaluationStore.updateData(action.updatedCells);
        // console.log("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
        ASEvaluationStore.emitChange();
        break;
      /*
        This action is sent to Dispatcher by the ASSpreadsheet action creator on a scroll event
        It gets previous scroll state from the store and then uses the API to send a "get cells" message to server
      */
      case Constants.ActionTypes.SCROLLED:
        API.updateViewingWindow(action.vWindow);
        break;
      /*
        The cells have been fetched from the server for a get request (for example, when scrolling)
        We now need to update the store based on these new values
        Called from Dispatcher, fired by API response from server
      */
      case Constants.ActionTypes.FETCHED_CELLS:
        _data.lastUpdatedCells = [];
        ASEvaluationStore.updateData(action.newCells);
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
        ASEvaluationStore.removeData(cellsToRemove);
        _data.allCells = {};
        // console.log("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
        ASEvaluationStore.emitChange();
        break;
      case Constants.ActionTypes.DELETED_LOCS:
        console.log("deleting locs from store: " + JSON.stringify(action.locs));
        ASEvaluationStore.removeLocs(action.locs);
        ASEvaluationStore.emitChange();
        break;
      case Constants.ActionTypes.GOT_FAILURE:
        console.log("setting external error");
        ASEvaluationStore.setExternalError(action.errorMsg.result.failDesc);
        if (action.errorMsg.action === "EvaluateRepl"){
          console.log("repl error!");
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
  setCurrentSheetById(sheetid) {
    _data.currentSheet = {sheetId: sheetid, sheetName: "", sheetPermissions: {
      tag: 'Blacklist',
      contents: []
    }};
  },

  // Requires that a range having row2 implies a range has col2, and vice versa
  setActiveSelection(rng, xp){
    _data.activeSelection = rng;
    _data.activeCell = this.getCellAtLoc(rng.col, rng.row) || Converter.defaultCell();
    var activeCellDependencies = Util.parseDependencies(xp);
    if (rng.hasOwnProperty('row2') && rng.hasOwnProperty('col2')) {
      for (var r = rng.row; r <= rng.row2; ++r){
        for (var c = rng.col; c <= rng.col2; ++c) {
          // activeCellDependencies.push(this.getParentList(r, c));
        }
      }
    }
    this.setActiveCellDependencies(activeCellDependencies);
  },

  getParentList(r,c){
    if(this.getCellAtLoc(r,c) != null) {
      var ctags = this.getCellAtLoc.cellTags;
      for (var i = 0; i < ctags.length; ++i) {
        if (ctags[i].hasOwnProperty('listKey')){
          var listHead = Util.listKeyToListHead(ctags[i].listKey);
          var listDimensions = Util.listKeyToListDimensions(ctags[i].listKey);
          return {
            row: listHead.row,
            col: listHead.col,
            row2: listHead.row + listDimensions.row - 1,
            col2: listHead.col + listDimensions.col - 1
          }
        }
      }
    }
    return {row: r, column: c}
  },
    

//  setActiveSelection(rng, xp) {
//    _data.activeSelection = rng;
//    _data.activeCell = this.getCellAtLoc(rng.col, rng.row) || Converter.defaultCell();
//    this.setActiveCellDependencies(Util.parseDependencies(xp));
//  },
  getActiveSelection() {
    return _data.activeSelection;
  },
  getActiveCell() {
    return _data.activeCell;
  },
  setClipboard(rng, isCut) {
    console.log("setting clipboard: "+ JSON.stringify(rng));
    _data.clipboard.range = rng;
    _data.clipboard.isCut = isCut;
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
    // console.log("Getting last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
    return _data.lastUpdatedCells;
  },
  resetLastUpdatedCells() {
    _data.lastUpdatedCells = [];
  },
  addTag(tag, col, row) {
    let sheetid = _data.currentSheet.sheetId;
    if (_data.allCells[sheetid] && _data.allCells[sheetid][col] && _data.allCells[sheetid][col][row]){
      _data.allCells[sheetid][col][row].cellTags.push(tag);
      API.sendTagsMessage("AddTags", [tag], col, row);
    }
  },
  setExternalError(err) {
    _data.externalError = err;
  },
  getExternalError(){
    return _data.externalError;
  },


  /**************************************************************************************************************************/
  /*
    Update methods to allCells and lastUpdatedCells.
    A cell in this class and stored in _data has the format from CellConverter, returned from eval
  */

  /* Function to update cell related objects in store. Caller's responsibility to clear lastUpdatedCells if necessary */
  updateData(cells) {
    console.log("About to update data in store: " + JSON.stringify(cells));
    for (var key in cells){
      let c = cells[key];
      let sheetid = Converter.clientCellGetSheetId(c);
      let col = Converter.clientCellGetCol(c);
      let row = Converter.clientCellGetRow(c);
      let xp = Converter.clientCellGetExpressionObj(c);
      let val = Converter.clientCellGetValueObj(c);
      if (!_data.allCells[sheetid])
        _data.allCells[sheetid] = [];
      if (!_data.allCells[sheetid][col])
        _data.allCells[sheetid][col] = [];
      _data.allCells[sheetid][col][row] = c;
      _data.lastUpdatedCells.push(c);
    }
  },

  /* Replace cells with empty ones. Caller's responsibility to clear lastUpdatedCells if necessary */
  removeData(cells) {
    console.log("About to remove data in store: " + JSON.stringify(cells));
    for (var key in cells){
      let c = cells[key];
      console.log("deleting cell: " + JSON.stringify(c));
      let sheetid = Converter.clientCellGetSheetId(c);
      let col = Converter.clientCellGetCol(c);
      let row = Converter.clientCellGetRow(c);
      if (!_data.allCells[sheetid])
        continue;
      if (!_data.allCells[sheetid][col])
        continue;
      _data.allCells[sheetid][col][row] = null;
      let emptyCell = Converter.clientCellEmpty(c.cellLocation);
      _data.lastUpdatedCells.push(emptyCell);
    }
  },

  removeLocs(locs) {
    let dlocs = Util.getDegenerateLocs(locs);
    console.log("removing locs: " + JSON.stringify(dlocs));
    for (var key in dlocs){
      let l = dlocs[key];
      if (!_data.allCells[l.locSheetId])
        continue;
      if (!_data.allCells[l.locSheetId][l.index.col])
        continue;
      _data.allCells[l.locSheetId][l.index.col][l.index.row] = null;
      let emptyCell = Converter.clientCellEmpty(l);
      _data.lastUpdatedCells.push(emptyCell);
    }
  },

  clearSheetCacheById(sheetid) {
    _data.allCells[sheetid] = null;
  },

  setActiveCellDependencies(deps) {
    _data.activeCell.cellExpression.dependencies = deps;
  },

  locationExists(sheetid, col, row) {
    return (_data.allCells[sheetid] && _data.allCells[sheetid][col] && _data.allCells[sheetid][col][row]);
  },

  /**************************************************************************************************************************/
  /* Updating expression when user clicks on a cell */

  getCellAtLoc(col,row){
    let sheetid = _data.currentSheet.sheetId;
    if (this.locationExists(sheetid, col, row))
      return _data.allCells[sheetid][col][row];
    else {
      return null;
    }
  },

  getCellAtLocSheet(sheetid, col,row){
    if (this.locationExists(sheetid, col, row))
      return _data.allCells[sheetid][col][row];
    else {
      return null;
    }
  },

  /**************************************************************************************************************************/

  /*
    Sets invisible rows in cache to null to limit memory usage
    TODO overlapping corners not handled... determine better way to dealloc than casework
  */
  // NOT RIGHT
  deallocAfterScroll(newX, newY, oldX, oldY, vWindow) {
    let eX = Constants.scrollCacheX,
        eY = Constnts.scrollCacheY;
        sheet = _data.currentSheet;
    /* scroll right */
    if (oldX < newX) {
      for (var c = oldX - eX; c < newX - eX; c ++)
        _data.allCells[sheet][c] = null;
        /*if (_data.allCells[sheet][c])
          for (var r = oldY - eY; i < oldY + vWindow.height + eX; r++)
            _data.allCells[sheet][c][r] = null; */
    }
    /* scroll left */
    else if (oldX > newX) {
      for (var c = newX + eX; x < oldX + eX; c++)
        _data.allCells[sheet][c] = null;
        /* if (_data.allCells[sheet][c])
          for (var r = oldY - eY; i < oldY + vWindow.height + eX; r++)
            _data.allCells[sheet][c][r] = null; */
    }
    /* scroll down */
    if (newY > oldY) { // scroll down
      for (var r = oldY - eY; r < newY - eY; r++)
        _data.allCells[r] = null;
    } else if (newY < oldY) { // up
      for (var r = newY - eY; r < oldY - eY; r++)
        _data.allCells[r] = null;
    }
  }

});


export default ASEvaluationStore;
