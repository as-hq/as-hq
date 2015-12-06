/* @flow */

import {logDebug} from '../AS/Logger';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import Util from '../AS/Util';

let _data = {
  initColumnProps: ([]: Array<Array<number>>)
};

const ASInitRowColPropsStore = Object.assign({}, BaseStore, {

  /* This function describes the actions of the ASReplStore upon recieving a message from Dispatcher */
  dispatcherIndex: Dispatcher.register(function (action) {
    switch (action._type) {
      case 'GOT_OPEN':
        ASInitRowColPropsStore.setInitColumnProps(action.initColumnProps);
        ASInitRowColPropsStore.emitChange();
        break;
    }
  }),

  getInitColumnProps(): Array<Array<number>> {
    return _data.initColumnProps;
  },

  setInitColumnProps(initColumnProps: Array<Array<number>>) {
    _data.initColumnProps = initColumnProps;
  }
});

export default ASInitRowColPropsStore;
