import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
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
  workbooks: []
};

const ASEvaluationStore = assign({}, BaseStore, {
  /* This function describes the actions of the ASEvaluationStore upon recieving a message from Dispatcher */
  dispatcherIndex:
    Dispatcher.register(function (action) {
      switch (action.type) {
        case Constants.ActionTypes.GOT_UPDATED_WORKBOOKS:
          ASEvaluationStore.updateData(action.workbooks);
          ASEvaluationStore.emitChange();
      }
    }),

  updateData(wbs) {
    _data.workbooks = wbs;
  },

  getWorkbooks() {
    return _data.workbooks;
  },

  getSheets(workbookId) {
    return _data.workbooks[workbookId].sheets;
  }
});


export default ASEvaluationStore;
