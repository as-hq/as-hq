import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import ExpStore from './ASExpStore';
import Util from '../AS/Util';

// This is the uid of the open dropdown. Null means that none are open. Also keep track of source of change. 
let _data = {
  lastClickedId: null,
  origin: null
};

const ASToolbarStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register(action => {
    _data.origin = action._type;
    switch (action._type){
      case Constants.ActionTypes.LANGUAGE_TOGGLED:
        // Change ExpStore lang and default lang, and emitChange for LanguagePicker to pick up on
        ExpStore.toggleLanguage(action.lang);
        ASToolbarStore.emitChange();
        break;
      default:
        // Keep track of uid  of last open dropdown
        if (!action.visible) {
          _data.lastClickedId = null;
        } else {
          _data.lastClickedId = action.id;
        }
        debugger;
        ASToolbarStore.emitChange();
    }
  }),
  getLastClickedId() {
    return _data.lastClickedId;
  },
  getOrigin(){
    return _data.origin;
  }
});


export default ASToolbarStore;
