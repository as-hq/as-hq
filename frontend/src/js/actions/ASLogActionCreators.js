// @flow

import type {
	ASAction
} from '../types/Actions';

import ws from '../AS/PersistentWebSocket';
import Constants from '../Constants';
import shortid from 'shortid';

import pws from '../AS/PWSInstance';

export function log(action: ASAction) {
  // if (! nonLoggedActions.includes(action._type) && !! window.isLoggedIn) {
  //   const msg = {
  //     tag: "LogAction",
  //     contents: JSON.stringify(action)
  //   };
  //   const messageId = shortid.generate();
  //   const serverMsg = {
  //     serverAction: msg,
  //     messageId
  //   };
  //   pws.send(serverMsg);
  // }
}

// do not log these actions.
const nonLoggedActions = [
  'SET_CONNECTING_STATE',
  'LOGIN_ATTEMPT',
  'LOGIN_FAILURE',
  'LOGIN_SUCCESS'
];
