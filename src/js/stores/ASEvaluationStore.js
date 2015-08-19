import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';
import API from '../actions/ASApiActionCreators';
import CellConverter from '../AS/CellConverter';

// indexed by row/column
let _data = {
  sheet: "Demo",
  allCells: {},
  lastUpdatedCells: [],
  xscroll: 0,
  yscroll: 0
};

const ASEvaluationStore = assign({}, BaseStore, {

  setSheet(sht) {
    _data.sheet = sht;
  },
  getSheet() {
    return _data.sheet;
  },
  setScroll(x, y){
    _data.xscroll = x;
    _data.yscroll = y;
  },
  getScroll() {
    return {x: _data.xscroll, y: _data.yscroll};
  },

  getLocationHash(cell){
    return cell.cellLocation.index.col + cell.cellLocation.sheet + cell.cellLocation.index.row;
  },

  hash(cell){
    return cell.cellLocation.index[0] + cell.cellLocation.sheet + cell.cellLocation.index[1];
  },

  // updates _allCells and _lastUpdatedCells after eval returns a list of ASCells
  updateData(cells) {
    console.log("About to update data in store: " + JSON.stringify(cells));
    _data.lastUpdatedCells = [];
    for (var key in cells){
      _data.allCells[this.getLocationHash(cells[key])] = cells[key];
      _data.lastUpdatedCells.push(cells[key]);
    }
  },
  insData(cells) {
    for (var key in cells){
      let c = CellConverter.fromServerCell(cells[key]);
      console.log("Inserting: " + JSON.stringify(c));
      _data.allCells[this.getLocationHash(c)] = c;
      _data.lastUpdatedCells.push(c);
    }
  },
  removeData(cells) {
    _data.lastUpdatedCells = [];
    for (var key in cells){
      let c = CellConverter.fromServerCell(cells[key]);
      console.log("Removing: " + JSON.stringify(c));
      let ce = {"tag":"Expression","expression":"","language":c.cellExpression.language}; 
      let cv = {"tag":"ValueS","contents":""};
      let emptyCell = {cellLocation:c.cellLocation,cellExpression:ce,cellValue:cv};
      _data.allCells[this.getLocationHash(c)] = emptyCell;
      _data.lastUpdatedCells.push(emptyCell);
    }
    this.showAllCells();
    this.showUpdatedCells(); 
  },

  getLastUpdatedCells(){
    console.log("Getting last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
    return _data.lastUpdatedCells;
  },

  showAllCells(){
    console.log("All cells: " + JSON.stringify(_data.allCells));
  },
  showUpdatedCells(){
    console.log("Last updated cells: " + JSON.stringify(_data.lastUpdatedCells));
  },

  // used for updating expression when user clicks on a cell
  getExpressionAtLoc(loc){
    if (_data.allCells[loc.row] && _data.allCells[loc.row][loc.col])
      return _data.allCells[loc.row][loc.col].cellExpression;
    else {
      // console.log("cell not present at loc: "+JSON.stringify(loc));
      return {expression: '', language: 'python'};
    }
  },

  // sets invisible rows in cache to null
  // to limit memory usage
  // TODO columns also, if actually necessary
  deallocAfterScroll(newX, newY, oldX, oldY, vWindow) {
    let eX = Constants.scrollCacheX,
        eY = Constants.scrollCacheY;
    if (oldX < newX) { // scroll right
      for (var r = oldY - eY; i < oldY + vWindow.height + eX; r++)
        if (_data.allCells[r])
          for (var c = oldX - eX; c < newX - eX; c ++)
            _data.allCells[r][c] = null;
    } else if (oldX > newX) { // left
      for (var r = oldY - eY; i < oldY + vWindow.height + eX; r++)
        if (_data.allCells[r])
          for (var c = newX + eX; x < oldX + eX; c++)
            _data.allCells[r][c] = null;
    }
    if (newY > oldY) { // scroll down
      for (var r = oldY - eY; r < newY - eY; r++)
        _data.allCells[r] = null;
    } else if (newY < oldY) { // up
      for (var r = newY - eY; r < oldY - eY; r++)
        _data.allCells[r] = null;
    }
  }


});

/* This function describes the actions of the ASEvaluationStore upon recieving a message from Dispatcher */
dispatcherIndex: Dispatcher.register(function (action) {
    switch (action.type) {
      case Constants.ActionTypes.CELL_CHANGED: 
        break;
      case Constants.ActionTypes.RANGE_CHANGED:
        break;
      /*On an UNDO/REDO/UPDATE_CELLS, update the viewing window in the store based on the commit and 
      send a change event to spreadsheet, which will rerender */
      case Constants.ActionTypes.GOT_UNDO:
        _data.lastUpdatedCells = []; 
        ASEvaluationStore.removeData(action.commit.after);
        ASEvaluationStore.insData(action.commit.before); 
        ASEvaluationStore.emitChange(); 
        break;
      case Constants.ActionTypes.GOT_REDO:
        _data.lastUpdatedCells = []; 
        ASEvaluationStore.removeData(action.commit.before);
        ASEvaluationStore.insData(action.commit.after); 
        ASEvaluationStore.emitChange(); 
        break;
      case Constants.ActionTypes.GOT_UPDATED_CELLS:
        console.log("In updated cells dispatch register");
        ASEvaluationStore.updateData(action.updatedCells);
        ASEvaluationStore.emitChange();
        break;
      /*On a SCROLL, get the relevant cells from the database and update the eval store and
      send a change event to spreadsheet, which will rerender */
      case Constants.ActionTypes.SCROLLED:
        let {x,y} = ASEvaluationStore.getScroll();
        console.log("scrolling action: x "+ action.xscroll + ", y " + action.yscroll);
        let cells = API.getCellsForScroll(action.xscroll, action.yscroll, x, y, action.vWindow);
        ASEvaluationStore.setScroll(action.xscroll, action.yscroll);
        ASEvaluationStore.updateData(cells);
        ASEvaluationStore.emitChange();
        break;
    }
  })

export default ASEvaluationStore;
