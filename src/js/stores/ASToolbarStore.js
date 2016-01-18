import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import ExpStore from './ASExpStore';
import Util from '../AS/Util';

// This is the uid of the open dropdown. Null means that none are open. 
let _data = {
  lastClickedId: null
};

const ASToolbarStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register(action => {
    // Keep track of uid of last open dropdown
    if (!action.visible) {
      _data.lastClickedId = null;
    } else {
      _data.lastClickedId = action.id;
    }
    ASToolbarStore.emitChange();
  }),
  getLastClickedId() {
    return _data.lastClickedId;
  }
});


export default ASToolbarStore;
