import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';

// This is the name of the open dropdown. Null means that none are open.
const _data = {
  openItem: null,
};

const ASToolbarStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register(action => {
    switch (action._type) {
      case 'RESET':
        _data.openItem = null;
        break;
      case 'OPEN_TOOLBAR_ITEM':
        _data.openItem = action.name;
        ASToolbarStore.emitChange();
        break;
      case 'CLOSE_TOOLBAR_ITEM':
        if (_data.openItem !== null) {
          _data.openItem = null;
          ASToolbarStore.emitChange();
        }
        break;
      case 'SET_FORMAT':
        _data.openItem = null;
        ASToolbarStore.emitChange();
        break;
      default:
        break;
    }
  }),

  getLastClickedId() {
    return _data.lastClickedId;
  },

  getActiveMenuItem() {
    return _data.openItem;
  },
});


export default ASToolbarStore;
