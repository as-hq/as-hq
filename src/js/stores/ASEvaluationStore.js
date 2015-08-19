import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';
import API from '../actions/ASApiActionCreators';
import Converter from '../AS/Converter';

/* 
Private variable keeping track of a viewing window (cached) of cells. Stores:
  1) Sheet name
  2) All cells in viewing window, indexed [sheet][col][row]
  3) Cells that were last updated by an eval or change event (so that components can easily access the update from the store)
  4) Scroll position
*/
let _data = {
  currentSheet: "Demo",
  allCells: {}, 
  lastUpdatedCells: [],
  xscroll: 0,
  yscroll: 0
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
        console.log("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells)); 
        ASEvaluationStore.emitChange();
        break;
      /*
        This action is sent to Dispatcher by the ASSpreadsheet action creator on a scroll event
        It gets previous scroll state from the store and then uses the API to send a "get cells" message to server
      */
      case Constants.ActionTypes.SCROLLED:
        let {x,y} = ASEvaluationStore.getScroll(); // current values of scroll position
        console.log("scrolling action: x "+ action.xscroll + ", y " + action.yscroll);
        let cells = API.sendGetRequestScroll(action.xscroll, action.yscroll, x, y, action.vWindow);
        ASEvaluationStore.setScroll(action.xscroll, action.yscroll);
        ASEvaluationStore.updateData(cells);
        ASEvaluationStore.emitChange();
        break;
      /*
        The cells have been fetched from the server for a get request (for example, when scrolling)
        We now need to update the store based on these new values
        Called from Dispatcher, fired by API response from server
      */
      case Constants.ActionTypes.FETCHED_CELLS:
        _data.lastUpdatedCells = []; 
        ASEvaluationStore.updateData(action.newCells);
        console.log("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells)); 
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
        console.log("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells)); 
        ASEvaluationStore.emitChange();
        break;
      }
  })


const ASEvaluationStore = assign({}, BaseStore, {

  /**************************************************************************************************************************/
  /* Sheet and scroll getter and setter methods */

  setSheet(sht) {
    _data.currentSheet = sht;
  },
  getSheet() {
    return _data.currentSheet;
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
    console.log("Getting last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
    return _data.lastUpdatedCells;
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
      let sheet = Converter.clientCellGetSheet(c); 
      let col = Converter.clientCellGetCol(c);
      let row = Converter.clientCellGetRow(c);
      let xp = Converter.clientCellGetExpressionObj(c);
      let val = Converter.clientCellGetValueObj(c);
      if (!_data.allCells[sheet])
        _data.allCells[sheet] = []; 
      if (!_data.allCells[sheet][col])
        _data.allCells[sheet][col] = []; 
      _data.allCells[sheet][col][row] = c;
      _data.lastUpdatedCells.push(c);
    }
  },
  
  /* Replace cells with empty ones. Caller's responsibility to clear lastUpdatedCells if necessary */
  removeData(cells) {
    console.log("About to remove data in store: " + JSON.stringify(cells));
    for (var key in cells){
      let c = cells[key];
      let sheet = Converter.clientCellGetSheet(c); 
      let col = Converter.clientCellGetCol(c);
      let row = Converter.clientCellGetRow(c);
      let emptyCell = Converter.clientCellEmptyVersion(c); 
      if (!_data.allCells[sheet])
        continue;      
      if (!_data.allCells[sheet][col])
        continue; 
      _data.allCells[sheet][col][row] = emptyCell;
      _data.lastUpdatedCells.push(emptyCell);
    }
  },

  /**************************************************************************************************************************/
  /* Updating expression when user clicks on a cell */

  getCellAtLoc(col,row){
    let currentSheet = this.getSheet(); 
    if (_data.allCells[currentSheet] && _data.allCells[currentSheet][col] && _data.allCells[currentSheet][col][row])
      return _data.allCells[currentSheet][col][row];
    else {
      return Converter.defaultCell();
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
