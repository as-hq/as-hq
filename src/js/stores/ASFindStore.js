import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';

let _data = {
  findText:'',
  findPos:0,
  findTotal:0,
  findLocs:[]
};

dispatcherIndex: Dispatcher.register(function (action) {
  switch (action._type) {
    case Constants.ActionTypes.GOT_FIND:
      ASFindStore.setFindLocs(action.findLocs);
      ASFindStore.setFindPos(0);
      ASFindStore.setFindTotal(action.findLocs.length);
      ASFindStore.emitChange();
      logDebug("Updated find store");
      break;
    case Constants.ActionTypes.FIND_INCREMENTED: //down or enter
      ASFindStore.increment();
      ASFindStore.emitChange();
      break;
    case Constants.ActionTypes.FIND_DECREMENTED: //up
      ASFindStore.decrement();
      ASFindStore.emitChange();
      break;
  }
});

const ASFindStore = Object.assign({}, BaseStore, {

  /**************************************************************************************************************************/
  // store modification methods

  getFindText() {
    return _data.findText;
  },
  setFindText(v) {
    _data.findText=v;
  },
  getFindPos() {
    return _data.findPos;
  },
  setFindPos(pos) {
    _data.findPos=pos;
  },
  getFindTotal() {
    return _data.findTotal;
  },
  setFindTotal(t) {
    _data.findTotal=t;
  },
  setFindLocs(locs) {
    _data.findLocs = locs;
  },
  getFindLocs() {
    return _data.findLocs;
  },
  increment() {
    if (ASFindStore.getFindTotal() !== 0) {
      let n = ASFindStore.getFindTotal(),
          p = ASFindStore.getFindPos()+1;
      ASFindStore.setFindPos(((p % n) + n) % n); // fucking js
    }
  },
  decrement() {
    if (ASFindStore.getFindTotal() !== 0) {
      let n = ASFindStore.getFindTotal(),
          p = ASFindStore.getFindPos()-1;
      ASFindStore.setFindPos(((p % n) + n) % n);
    }
  }


});


export default ASFindStore;
