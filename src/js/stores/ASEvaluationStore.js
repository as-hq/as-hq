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
  partialSelections: [],
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

        // remove possibly null cells
        cellsToRemove = cellsToRemove.filter((cell) => !!cell);

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
  setActiveSelection(area, xp) {
    let rng = area.range;
    _data.activeSelection = area;
    _data.activeCell = this.getCellAtLoc(rng.col, rng.row) || Converter.defaultCell();
    var activeCellDependencies = Util.parseDependencies(xp);
    if (rng.hasOwnProperty('row2') && rng.hasOwnProperty('col2')) {
      for (var r = rng.row; r <= rng.row2; ++r){
        for (var c = rng.col; c <= rng.col2; ++c) {
          activeCellDependencies.push(this.getParentList(c, r));
        }
      }
    }
    else {
      c = rng.col;
      r = rng.row;
      activeCellDependencies.push(this.getParentList(c, r));
    }
    this.setActiveCellDependencies(activeCellDependencies);
  },

  getParentList(c,r){
    if (this.getCellAtLoc(c, r) == null) {
      // console.log("timchu: There is no cell at location " + r + ", " + c);
      return {row: r, column: c};
    }
    console.log(this.getCellAtLoc(c,r));
    var ctags = this.getCellAtLoc(c,r).cellTags;
    if (ctags == undefined) {
      console.log("timchu: The ctags are undefined at " + r + ", " + c);
      return {row: r, column: c};
    }
    for (var i = 0; i < ctags.length; ++i) {
      console.log(ctags[i]);
      console.log(ctags[i].hasOwnProperty('listKey'));
      if (ctags[i].hasOwnProperty('listKey')){
        var listHead = Util.listKeyToListHead(ctags[i].listKey);
        var listDimensions = Util.listKeyToListDimensions(ctags[i].listKey);
        console.log( {  row: listHead.snd,
          col: listHead.fst,
          row2: listHead.snd + listDimensions.fst - 1,
          col2: listHead.fst + listDimensions.snd - 1 } );
        return {
          row: listHead.snd,
          col: listHead.fst,
          row2: listHead.snd + listDimensions.fst - 1,
          col2: listHead.fst + listDimensions.snd - 1
        }
      }
    }
    console.log("No listkey tags");
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
  getActiveCellDependencies() {
    return(_data.activeCell.cellExpression.dependencies);
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
  /* Copy paste helpers */

   //Utils for selRegionToValues
   sliceArray (begin, end) {
     return (
         function(arr) { return arr.slice(begin, end) } 
     );
   },

   clientCellToValue(clientCell) {
     let v = clientCell.cellValue.contents;
     if (v) { // non ValueError value
       if (v.constructor === Array) {
         if (v.length !== 0){
          console.log("Returning value: " +v[0]);
           return v[0];
         }
         return "";
       }
      if (clientCell.cellValue.hasOwnProperty("contents")){
         return clientCell.cellValue.contents;
      }
    } else if (clientCell.cellValue.errMsg) { 
      return "ERROR"; // TODO: display different types of errors depending on the type
    }
    return ""; 
   },

   invertArray(array){
    return array[0].map(function(col, i) { 
      return array.map(function(row) { 
        return row[i] 
      })
    });
   },

   // Converts a range to a row major list of lists, 
   selRegionToValues(rng){
     console.log("SEL REGION RNG " + JSON.stringify(rng));
     let sheetid = _data.currentSheet.sheetId;
     let col = rng.col, row = rng.row;
     if (this.locationExists(sheetid, col, row)) {
       if (!rng.row2) {
        console.log(this.clientCellToValue(this.getCellAtLoc(col, row)));
        return [[this.clientCellToValue(this.getCellAtLoc(col, row))]];
      }
       else if (this.locationExists(sheetid, rng.col2, rng.row2)) {
         let col2 = rng.col2, row2 = rng.row2;
         let colMajorCells = _data.allCells[sheetid].slice(col, col2+1).map(this.sliceArray(row, row2+1));
         let self = this;
         let colMajorValues = colMajorCells.map(function(col){
          return col.map(self.clientCellToValue);
         });
         return this.invertArray(colMajorValues);
       }
     }
     return null;
   },

   // Methods for paste
   makeServerCell(loc, language, i, j) {
    let sheet = _data.currentSheet.sheetId;
     return function(v) {
       let row = loc.range.row, col = loc.range.col;
       return  {
         "cellLocation": {
          locSheetId: sheet,
          index: [col + j, row + i]
        },
         "cellExpression": {
           "expression" : v,
           "language": language.Server
         },
         "cellValue":{
           "tag": "NoValue",
           "contents": []
         },
         "cellTags": []
       };
     };
   },

   arrayToASCells(loc, language) {
    var self = this;
     return function(i){
       return function(v, j) {
         return self.makeServerCell(loc, language, i, j)(v);
       };
     };
   },

   rowValuesToASCells(loc, language){ 
    var self = this;
     return function(values, i){
       return values.map(self.arrayToASCells(loc, language)(i));
     };
   },

   makeASCellsFromVals(loc, vals, language) {
     return vals.map(this.rowValuesToASCells(loc, language));
   },


  /**************************************************************************************************************************/
  /*
    Update methods to allCells and lastUpdatedCells.
    A cell in this class and stored in _data has the format from CellConverter, returned from eval
  */

  /* Function to update cell related objects in store. Caller's responsibility to clear lastUpdatedCells if necessary */
  updateData(cells) {
    console.log("About to update data in store: " + JSON.stringify(cells));
    let removeCells = [];
    for (var key in cells){
      let c = cells[key];
      let sheetid = Converter.clientCellGetSheetId(c);
      let col = Converter.clientCellGetCol(c);
      let row = Converter.clientCellGetRow(c);
      let xp = Converter.clientCellGetExpressionObj(c);
      let val = Converter.clientCellGetValueObj(c);

      if (xp.expression != "") {
        if (!_data.allCells[sheetid])
          _data.allCells[sheetid] = [];
        if (!_data.allCells[sheetid][col])
          _data.allCells[sheetid][col] = [];
        _data.allCells[sheetid][col][row] = c;
        _data.lastUpdatedCells.push(c);
      } else {
        removeCells.push(c); // filter out all the blank cells passed back from the store
      }
    }
    this.removeData(removeCells);
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
