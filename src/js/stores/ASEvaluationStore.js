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
    let origin = area.origin;
    _data.activeSelection = area;
    console.log("\n\norigin cell\n", this.getCellAtLoc(origin.col, origin.row));
    _data.activeCell = this.getCellAtLoc(origin.col, origin.row) || Converter.defaultCell();
    var activeCellDependencies = Util.parseDependencies(xp);
    let c = area.origin.col,
        r = area.origin.row,
        dep = this.getParentList(c, r);
    if (dep) {
      activeCellDependencies.push(dep);
    }
    this.setActiveCellDependencies(activeCellDependencies);
    console.log("\nDEPS\n", activeCellDependencies);
  },

  getParentList(c,r){
    let sheetid = _data.currentSheet.sheetId,
        thisExists = this.locationExists(sheetid, c, r),
        ctags = thisExists ? this.getCellAtLoc(c,r).cellTags : null;
    if (thisExists && ctags) {
      for (var i = 0; i < ctags.length; ++i) {
        if (ctags[i].hasOwnProperty('listKey')){
          let listHead = Util.listKeyToListHead(ctags[i].listKey),
              listDimensions = Util.listKeyToListDimensions(ctags[i].listKey);
          return {
            row: listHead.snd,
            col: listHead.fst,
            row2: listHead.snd + listDimensions.fst - 1,
            col2: listHead.fst + listDimensions.snd - 1
          }
        }
      }
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
    console.log("setting clipboard: "+ JSON.stringify(rng));
    _data.clipboard.area = rng;
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
  toggleTag(tag, rng) {
    let sheetid = _data.currentSheet.sheetId;
    API.sendTagsMessage("ToggleTags", [tag], rng); 


    if (_data.allCells[sheetid] && _data.allCells[sheetid][col] && _data.allCells[sheetid][col][row]){
      _data.allCells[sheetid][col][row].cellTags.push(tag);
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
    if (!clientCell) {
      return "";
    }

    let v = clientCell.cellValue.contents;

    if (v) { // non ValueError value
      if (v.constructor === Array){ // #needsrefactor (probably elsewhere in code): why are we treating x and [x] as the same?
        if (v.length !== 0){
          console.log("Returning value: " +v[0]);
          return v[0];
        }
        return "";
       }
      if (clientCell.cellValue.hasOwnProperty("contents")){
        console.log("Contents are : " + clientCell.cellValue.contents);
        return clientCell.cellValue.contents;
      }
      return "";
    } else if (clientCell.cellValue.errMsg) { // must be errror
      return "ERROR"; // TODO: display different types of errors depending on the type
    }
  },

   makeArrayOf(value, length) {
     var arr = [], i = length;
     while (i--) {
       arr[i] = value;
     }
     return arr;
   },

   make2DArrayOf(value, height, length) {
     var arr = [[]], i = height;
     while (i--) {
       arr[i] = this.makeArrayOf(value, length);
     }
     return arr;
   },

   // Converts a range to a row major list of lists of values,
   selRegionToValues(rng){
     console.log("Change registered");
     console.log("SEL REGION RNG " + JSON.stringify(rng));
     let sheetid = _data.currentSheet.sheetId;
     let col = rng.col, row = rng.row;
     if (!rng.row2) {
       console.log(this.clientCellToValue(this.getCellAtLoc(col, row)));
       return [[this.clientCellToValue(this.getCellAtLoc(col, row))]];
     }
     let col2 = rng.col2,
         row2 = rng.row2;
     let height = row2 - row + 1,
         length = col2 - col + 1;
     var rowMajorValues = this.make2DArrayOf("", height, length);
     for (let i = 0; i < height; ++i) {
       let currentRow = row + i;
       var self = this;
       rowMajorValues[i] = rowMajorValues[i].map(function(value, index) {
           let currentColumn = col + index;
           console.log(currentColumn  + " " + currentRow + " " + "IS CURRENT ROW COLUMN");
           console.log(sheetid);
           console.log("Hooha");
           if (self.locationExists(sheetid, currentColumn, currentRow)) {
             return self.clientCellToValue(_data.allCells[sheetid][currentColumn][currentRow]);
           }
           else {
             return "";
           }
       });
     }
     return rowMajorValues;
   },

   // TODO: move somewhere else maybe, in some global util method?
  _dispBoolInLang(b, lang) {
    if (b) {
      if (["R", "OCaml"].indexOf(lang) != -1) {
        return "true";
      } else {
        return "True";
      }
    } else {
      if (["R", "OCaml"].indexOf(lang) != -1) {
        return "false";
      } else {
        return "False";
      }
    }
    throw "Should never make it to the end of _dispBoolInLang";
   },

  _expressionFromValue(v, lang) {
    if (lang.Server == "Excel") { // is language.Editor the correct thing?
      return v;
    } else if (v != null && typeof(v) != "undefined") {
      if (!isNaN(Number(v))) {
        return v;
      } else if (v.toUpperCase() == "TRUE") {
        return this._dispBoolInLang(true, lang.Server);
      } else if (v.toUpperCase() == "FALSE") {
        return this._dispBoolInLang(false, lang.Server);
      } else if (v == "#REF!") {
        return v; 
      } else { 
        return JSON.stringify(v);
      }
    } else {
      return ""; // unclear if we ever get here -- Alex 10/19
    }
  },

   // Methods for paste
  _makeServerCell(loc, language, i, j) {
    let sheet = _data.currentSheet.sheetId;
    let self = this;
     return function(v) {
        let row = loc.range.row, col = loc.range.col;
        return {
          "cellLocation": {
            "tag": "Index",
            locSheetId: sheet,
            index: [col + j, row + i]
          },
          "cellExpression": {
            "tag": "Expression",
            "expression" : self._expressionFromValue(v, language),
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

   _arrayToASCells(loc, language) {
    var self = this;
     return function(i){
       return function(v, j) {
         return self._makeServerCell(loc, language, i, j)(v);
       };
     };
   },

   _rowValuesToASCells(loc, language){
    var self = this;
     return function(values, i){
       return values.map(self._arrayToASCells(loc, language)(i));
     };
   },

   // takes in a set of locations and the values at those locations,
   makeASCellsFromPlainVals(loc, vals, language) {
     return vals.map(this._rowValuesToASCells(loc, language));
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
        this.addCell(c, sheetid, col, row);
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

  getExtendedRange(direction, isShifted) {
    let sel = _data.activeSelection.range,
        origin = _data.activeSelection.origin,
        sheetId = _data.currentSheet.sheetId,
        selExists,
        startRow, startCol,
        hExtremum = origin.col > sel.col ? sel.col : (sel.col2 ? sel.col2 : sel.col),
        vExtremum = origin.row > sel.row ? sel.row : (sel.row2 ? sel.row2 : sel.row),
        result;
    // debugger;
    // console.log("\n\nin data bundary func\n\n", sel);
    console.log("\n\nhextremum\n\n", hExtremum);
    switch(direction) {
      case "Right":
        startCol = this.locationExists(sheetId, hExtremum+1, sel.row) ? hExtremum : hExtremum+1;
        selExists = this.locationExists(sheetId, startCol, sel.row);
        for (var col = startCol; col < startCol + Constants.LARGE_SEARCH_BOUND; col++){
          let thisExists = this.locationExists(sheetId, col, origin.row);
          if (Util.xor(selExists, thisExists)) {
            result = thisExists ? {col: col, row: sel.row} : {col: col-1, row: sel.row};
            let resultCol = origin.col > sel.col ? result.col : sel.col;
            let resultCol2 = origin.col > sel.col ? sel.col2 : result.col;
            result = !isShifted ? result : {row: sel.row,
                                            col: resultCol,
                                            row2: sel.row2 ? sel.row2 : sel.row,
                                            col2: resultCol2};
            break;
          }
        }
        result = result ? result : sel;
        break;
      case "Down":
        startRow = this.locationExists(sheetId, sel.col, vExtremum+1) ? vExtremum : vExtremum+1;
        selExists = this.locationExists(sheetId, sel.col, startRow);
        for (var row = startRow; row < startRow + Constants.LARGE_SEARCH_BOUND; row++){
          let thisExists = this.locationExists(sheetId, origin.col, row);
          if (Util.xor(selExists, thisExists)) {
            result = thisExists ? {col: sel.col, row: row} : {col: sel.col, row: row-1};
            let resultRow = origin.row > sel.row ? result.row : sel.row;
            let resultRow2 = origin.row > sel.row ? sel.row2 : result.row;
            result = !isShifted ? result : {row: resultRow,
                                            col: sel.col,
                                            row2: resultRow2,
                                            col2: sel.col2 ? sel.col2 : sel.col};
            break;
          }
        }
        result = result ? result : sel;
        break;
      case "Left":
        startCol = this.locationExists(sheetId, hExtremum-1, sel.row) ? hExtremum : hExtremum-1;
        selExists = this.locationExists(sheetId, startCol, sel.row);
        for (var col = startCol; col > 1; col--) {
          let thisExists = this.locationExists(sheetId, col, origin.row);
          if (Util.xor(selExists, thisExists)) {
            result = thisExists ? {col: col, row: sel.row} : {col: col+1, row: sel.row};
            let resultCol = origin.col > sel.col ? result.col : sel.col;
            let resultCol2 = origin.col > sel.col ? sel.col2 : result.col;
            result = !isShifted ? result : {row: sel.row, col: resultCol, row2: sel.row2 ? sel.row2 : sel.row, col2: resultCol2};
            break;
          }
        }
        result = result ? result : {row: sel.row,
                                    col: 1,
                                    row2: isShifted ? (sel.row2 ? sel.row2 : sel.row) : null,
                                    col2: isShifted ? sel.col : null};
        break;
      case "Up":
        startRow = this.locationExists(sheetId, sel.col, vExtremum-1) ? vExtremum : vExtremum-1;
        selExists = this.locationExists(sheetId, sel.col, startRow);
        for (var row = startRow; row > 1; row--) {
          let thisExists = this.locationExists(sheetId, origin.col, row);
          if (Util.xor(selExists, thisExists)) {
            result = thisExists ? {col: sel.col, row: row} : {col: sel.col, row: row+1};
            let resultRow = origin.row > sel.row ? result.row : sel.row;
            let resultRow2 = origin.row > sel.row ? sel.row2 : result.row;
            result = !isShifted ? result : {row: resultRow, col: sel.col, row2: resultRow2, col2: sel.col2 ? sel.col2 : sel.col};
            break;
          }
        }
        result = result ? result : {row: 1,
                                    col: sel.col,
                                    row2: isShifted ? sel.row : null,
                                    col2: isShifted ? (sel.col2 ? sel.col2 : sel.col) : null};
        break;
    }
    console.log("\n\nRESULT\n\n", result);
    return Util.getOrientedArea(result);
  },

  // TODO actually get the data boundaries by iterating, or something
  // (but as long as we're using LARGE_SEARCH_BOUND, this area is an upper bound)
  getDataBounds() {
    return {col: 1, row: 1, col2: Constants.LARGE_SEARCH_BOUND, row2: Constants.LARGE_SEARCH_BOUND};
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
