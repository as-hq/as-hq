// @flow

import type ASAction from '../types/ASAction';
import ws from '../AS/PersistentWebSocket';
import Constants from '../Constants';
import shortid from 'shortid';

import pws from '../AS/PWSInstance';

export function log(action: ASAction) {
  if (! nonLoggedActions.includes(action._type) && !! window.isLoggedIn) {
    const msg = {
      tag: "LogAction",
      contents: JSON.stringify(action)
    };
    const messageId = shortid.generate();
    const serverMsg = {
      serverAction: msg,
      messageId
    };
    pws.waitForConnection((innerClient: WebSocket) => {
      innerClient.send(JSON.stringify(serverMsg));
    });
  }
}

// do not log these actions.s
const nonLoggedActions = [
  'SET_CONNECTING_STATE',
  'LOGIN_ATTEMPT',
  'LOGIN_FAILURE',
  'LOGIN_SUCCESS'
];
