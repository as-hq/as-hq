/* @flow */

import type {Callback} from '../types/Base';

import Constants from '../Constants';
import {EventEmitter} from 'events';

export default Object.assign({}, EventEmitter.prototype, {
  // Allow Controller-View to register itself with store
  addChangeListener(callback: Callback) {
    this.on(Constants.CHANGE_EVENT, callback);
  },

  removeChangeListener(callback: Callback) {
    this.removeListener(Constants.CHANGE_EVENT, callback);
  },

  // triggers change listener above, firing controller-view callback
  emitChange() {
    this.emit(Constants.CHANGE_EVENT);
  }
});
