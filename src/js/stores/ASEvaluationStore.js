import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';
import API from '../actions/ASApiActionCreators';
import CellConverter from '../AS/CellConverter';

// indexed by row/column
let _data = {
  sheet: "Demo",
  allCells: [],
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

  // takes a cell and extracts the location
  getLocation(cell){
    return cell.cellLocation.index;
  },

  // takes a cell and extracts the expression
  getExpression(cell){
    return cell.cellExpression.expression;
  },

  // takes a cell and extracts the value
  getValue(cell){
    return cell.cellValue.contents;
  },

  // updates _allCells and _lastUpdatedCells after eval returns a list of ASCells
  updateData(cells) {
    console.log("cells: " + JSON.stringify(cells));

    _data.lastUpdatedCells = [];
    _data.lastUpdatedCells = [];
    for (var key in cells){
      let loc = this.getLocation(cells[key]);
      if (!_data.allCells[loc.row])
        _data.allCells[loc.row] = [];
      _data.allCells[loc.row][loc.col] = cells[key];
      _data.lastUpdatedCells.push(cells[key]);

      // TODO gc invisible cells in _data
    }
    console.log("updated all cells and last updated cells");
  },

  getLastUpdatedCells(){
    return _data.lastUpdatedCells;
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

dispatcherIndex: Dispatcher.register(function (action) {
    switch (action.type) {
      case Constants.ActionTypes.CELL_CHANGED: // user selected a new cell
        break;
      case Constants.ActionTypes.RANGE_CHANGED:
        break;
      case Constants.ActionTypes.SCROLLED:
        let {x,y} = ASEvaluationStore.getScroll();
        console.log("scrolling action: x "+ action.xscroll + ", y " + action.yscroll);
        let cells = API.getCellsForScroll(action.xscroll, action.yscroll, x, y, action.vWindow);
        // ASEvaluationStore.deallocAfterScroll(action.xscroll, action.yscroll, x, y, action.vWindow);
        ASEvaluationStore.setScroll(action.xscroll, action.yscroll);
        ASEvaluationStore.updateData(cells);
        ASEvaluationStore.emitChange();
        break;
      case Constants.ActionTypes.GOT_UPDATED_CELLS:
        console.log("in updated cells dispatch register");
        ASEvaluationStore.updateData(action.updatedCells);
        ASEvaluationStore.emitChange();
        break;
    }
  })

export default ASEvaluationStore;
