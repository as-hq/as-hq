import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import CellConverter from '../AS/CellConverter';

var ActionTypes = Constants.ActionTypes;
var host = "ws://localhost:5000";
var wss = new WebSocket(host);

// when the server responds with updates to make, call dispatcher to update stores, then view
wss.onmessage = function (event) {
    console.log("client received data from server: " + event.data);
    var cells = JSON.parse(event.data);
    Dispatcher.dispatch({
      type: ActionTypes.GOT_UPDATED_CELLS,
      updatedCells: cells
    });
};

export default {

  // this function submits an eval request to a websocket server
  sendEvalRequest(selRegion,editorState){
    console.log("in eval action creator");
    wss.send(JSON.stringify(CellConverter.toASCell(selRegion,editorState)));
  }

};
