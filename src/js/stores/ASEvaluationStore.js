import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';

let _data = {
  currentCell: {
    language: 'python',
    expression: '',
    location: [[-1, -1], [-1, -1]]
  },
  allCells: []
};

const ASEvaluationStore = assign({}, BaseStore, {
  getCurrentCell() {
    return _data.currentCell;
  },

  dispatcherIndex: Dispatcher.register(function (payload) {
    let action = payload.action;
    switch (action.type) {
      case Constants.ActionTypes.CELL_CHANGED:
        break;
      case Constants.ActionTypes.RANGE_CHANGED:
        break;
      case Constants.ActionTypes.SCROLLED:
        break;
    }
  })
});

export default ASEvaluationStore;
