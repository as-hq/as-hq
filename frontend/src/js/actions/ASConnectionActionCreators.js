/* @flow */
import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for connection state changes */

export function setConnectedState(state: boolean) {
  Dispatcher.dispatch({
   _type: Constants.ActionTypes.SET_CONNECTING_STATE,
    isConnected: state
  });
}
