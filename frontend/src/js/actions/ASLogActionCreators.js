// @flow

import type ASAction from '../types/ASAction';
import ws from '../AS/PersistentWebSocket';
import Constants from '../Constants';
import shortid from 'shortid';

let pws = require('./PWSInstance').pws;

export function log(action: ASAction) {
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

