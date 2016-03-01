import Constants from '../Constants';
import ws from '../AS/PersistentWebSocket';

console.log("GOT URL: " + Constants.getBackendUrl('ws', Constants.BACKEND_WS_PORT));
let pws: ws = new ws(Constants.getBackendUrl('ws', Constants.BACKEND_WS_PORT));
exports.pws = pws;