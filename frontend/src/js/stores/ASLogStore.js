/* @flow */

import _ from 'lodash';

import type {
  ASAction
} from '../types/Actions';

import {List, Map, Record, Record$Class} from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';

import dispatcher from '../Dispatcher';
import Constants from '../Constants';
import U from '../AS/Util';


type LogStoreData = Record$Class;
const LogStoreDataRecord = Record({
  logs: List(),
  sessions: Map(),
  isOpen: false
})

class LogStore extends ReduceStore<LogStoreData> {

  getInitialState(): LogStoreData {
    return new LogStoreDataRecord();
  }

  reduce(state: LogStoreData, action: ASAction): LogStoreData {
    switch (action._type) {
      case 'GOT_ALL_SESSIONS':
        const sessionMap = _.groupBy(action.sessions, 'seshUserId');
        return state.set('sessions', Map(sessionMap));
      case 'GOT_SESSION_LOG':
        console.log(action);
        return state.set('logs', List(action.sessionLog));
      case 'LOG_VIEWER_OPENED':
        return state.set('isOpen', true);
      default: {
        return state;
      }
    }
  }

  getSessionLogs() {
    return this.getState().get('logs');
  }

  getIsOpen() {
    return this.getState().get('isOpen');
  }

  getAllSessions() {
    return this.getState().get('sessions');
  }

}

export default new LogStore(dispatcher);
