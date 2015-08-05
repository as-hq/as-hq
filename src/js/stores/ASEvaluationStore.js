import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';

// indexed by row/column
var _allCells = [[]]; // don't worry about other sheets/users for now

var _lastUpdatedCells = [];

const ASEvaluationStore = assign({}, BaseStore, {

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
    return cell.cellValue;
  },

  // updates _allCells and _lastUpdatedCells after eval returns a list of ASCells
  updateState(cells) {
    var i = 0 ;
    _lastUpdatedCells = [];
    for (; i < cells.length; i++){
      var loc = this.getLocation(cells[i]);
      var exp = this.getExpression(cells[i]);
      var val = this.getValue(cells[i]);
      if (!_allCells[loc[1]])
        _allCells[loc[1]] = []
      _allCells[loc[1]][loc[0]] = {expression: exp, val};
      _lastUpdatedCells.push({loc: loc, expression: exp, value: val})
    }
    console.log("updated all cells and last updated cells");
  },

  getLastUpdatedCells(){
    return _lastUpdatedCells;
  }

});

ASEvaluationStore.dispatchToken = Dispatcher.register(function (action) {
    switch (action.type) {
      case Constants.ActionTypes.CELL_CHANGED: // user selected a new cell
        break;
      case Constants.ActionTypes.RANGE_CHANGED:
        break;
      case Constants.ActionTypes.SCROLLED:
        break;
      case Constants.ActionTypes.GOT_UPDATED_CELLS:
        console.log("in updated cells dispatch register");
        ASEvaluationStore.updateState(action.updatedCells);
        ASEvaluationStore.emitChange();
        break;
    }
  });

export default ASEvaluationStore;
