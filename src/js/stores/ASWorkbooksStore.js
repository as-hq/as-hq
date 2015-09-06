import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';
import API from '../actions/ASApiActionCreators';
import Converter from '../AS/Converter';
import Util from '../AS/Util';

let _data = {
  workbooks: []
};

const ASWorkbookStore = assign({}, BaseStore, {
  /* This function describes the actions of the ASWorkbookStore upon recieving a message from Dispatcher */
  dispatcherIndex:
    Dispatcher.register(function (action) {
      switch (action.type) {
        case Constants.ActionTypes.GOT_UPDATED_WORKBOOKS:
          ASWorkbookStore.updateData(action.workbooks);
          ASWorkbookStore.emitChange();
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


export default ASWorkbookStore;
