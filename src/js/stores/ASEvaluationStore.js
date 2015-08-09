import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';

// indexed by row/column
let _data = {
  allCells: [[]],
  lastUpdatedCells: []
};

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
    return cell.cellValue.contents;
  },

  // updates _allCells and _lastUpdatedCells after eval returns a list of ASCells
  updateData(cells) {
    console.log("cells: " + JSON.stringify(cells));

    _data.lastUpdatedCells = [];
    _data.lastUpdatedCells = [];
    for (var key in cells){
      let loc = this.getLocation(cells[key]);
      console.log("updating loc: "+JSON.stringify(loc));
      if (!_data.allCells[loc[1]])
        _data.allCells[loc[1]] = []
      _data.allCells[loc[1]][loc[0]] = cells[key];
      _data.lastUpdatedCells.push(cells[key])
    }
    console.log("updated all cells and last updated cells");
  },

  getLastUpdatedCells(){
    return _data.lastUpdatedCells;
  },

  // used for updating expression when user clicks on a cell
  getExpressionAtLoc(loc){
    if (_data.allCells[loc[1]] && _data.allCells[loc[1]][loc[0]])
      return _data.allCells[loc[1]][loc[0]].cellExpression;
    else {
      console.log("cell not present at loc: "+JSON.stringify(loc));
      return {expression: '', language: 'python'};
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
        break;
      case Constants.ActionTypes.GOT_UPDATED_CELLS:
        console.log("in updated cells dispatch register");
        ASEvaluationStore.updateData(action.updatedCells);
        ASEvaluationStore.emitChange();
        break;
    }
  })

export default ASEvaluationStore;
