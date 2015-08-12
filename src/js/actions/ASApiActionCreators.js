import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import CellConverter from '../AS/CellConverter';

var ActionTypes = Constants.ActionTypes;
var wss = new WebSocket(Constants.host_ws);

// when the server responds with updates to make, call dispatcher to update stores, then view
wss.onmessage = function (event) {
    console.log("client received data from server: " + event.data);
    let msg = JSON.parse(event.data);
    let cells = CellConverter.getCellsFromMsg(msg);
    if (cells)
      Dispatcher.dispatch({
        type: ActionTypes.GOT_UPDATED_CELLS,
        updatedCells: CellConverter.getCellsFromMsg(msg)
      });
};

export default {

  // this function submits an eval request to a websocket server
  sendEvalRequest(selRegion,editorState){
    console.log("in eval action creator");
    let cell = CellConverter.toASCell(selRegion,editorState);
    let msg = CellConverter.toServerEvalFormat(cell)
    console.log('sending msg to server: ' + JSON.stringify(msg))
    wss.send(JSON.stringify(msg));
  }

};
