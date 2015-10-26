import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import assign from 'object-assign';
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
          break;
        case Constants.ActionTypes.GOT_NEW_WORKBOOKS:
          ASWorkbookStore.mergeWorkbooks(action.workbooks);
          ASWorkbookStore.emitChange();
          break;
        case Constants.ActionTypes.DELETED_WORKBOOKS:
          ASWorkbookStore.deleteWorkbooks(action.workbooks);
      }
    }),

/**************************************************************************************************************************/
// store modification methods

  updateData(wbs) {
    _data.workbooks = wbs;
  },

  mergeWorkbooks(wbs) {
    for (var key in wbs) {
      let wb = wbs[key];
      if (_data.workbooks[key]){
        // assumes the sheets are actually new
        _data.workbooks[key].wsSheets =
            Util.mergeSheets(_data.workbooks[key].wsSheets,
              wb.wsSheets);
      } else
        _data.workbooks[key] = wbs[key];
    }
  },

  deleteWorkbooks(wbs) {
    // TODO
    for (var key in wbs) {
      if (_data.workbooks[key]){
        _data.workbooks[key] =
            Util.removeSheets(_data.workbooks[key].wsSheets,
              wbs[key].wsSheets);
      }
    }
  },

  getWorkbooks() {
    return _data.workbooks;
  },

  getSheets(workbookId) {
    return _data.workbooks[workbookId].sheets;
  }
});


export default ASWorkbookStore;
