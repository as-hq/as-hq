import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import Util from '../AS/Util';

let _data = {
  isConnected: true
};

const ASConnectionStore = Object.assign({}, BaseStore, {

  dispatcherIndex: Dispatcher.register(action => {
    switch(action._type) {
      case 'SET_CONNECTING_STATE':
        _data.isConnected = action.isConnected;
        ASConnectionStore.emitChange();
        break;
    }
  }),

  getIsConnected() {
    return _data.isConnected;
  }
});


export default ASConnectionStore;
