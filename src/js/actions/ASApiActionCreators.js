import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import Converter from '../AS/Converter';

var ActionTypes = Constants.ActionTypes;
var wss = new WebSocket(Constants.HOST_WS);

/**************************************************************************************************************************/

/* 
  This action creator class serves two purposes
  1) Create messages for the server and send them
  2) Take messages received from the server and send them to dispatch
*/

/**************************************************************************************************************************/
/* 
  Called whenever the server returns a message 
  Depending on the action type of the message, calls dispatcher differently to propagate to stores
  Converts server to client types before going further
*/

wss.onmessage = function (event) {
  console.log("Client received data from server: " + JSON.stringify(event.data));
  let msg = JSON.parse(event.data);
  switch (msg.action) {
    case "Acknowledge":
      break;
    case "Undo":
      Dispatcher.dispatch({
        type: ActionTypes.GOT_UNDO,
        commit: Converter.serverToClientCommit(msg.payload.contents)
      });
      break;
    case "Redo":
      Dispatcher.dispatch({
        type: ActionTypes.GOT_REDO,
        commit: Converter.serverToClientCommit(msg.payload.contents)
      });
     break;
    case "Evaluate":
      let cells = Converter.clientCellsFromServerMessage(msg);
      Dispatcher.dispatch({
        type: ActionTypes.GOT_UPDATED_CELLS,
        updatedCells: cells
      });
      break;
    case "NoAction":
      break;
    case "Get":
      let newCells = Converter.clientCellsFromServerMessage(msg); // MAY NEED TO REPLACE 
      Dispatcher.dispatch({
        type: ActionTypes.FETCHED_CELLS,
        newCells: newCells
      });
      break; 
  }
};

export default {

  /**************************************************************************************************************************/
  /* Sending acknowledge message to server */

  sendInitialMessage(userName){
    let msg = Converter.toServerMessageFormat(Constants.ServerActions.Acknowledge,"PayloadInit",{userName:userName});
    console.log("Sending init message: " + JSON.stringify(msg)); 
    this.waitForSocketConnection(wss,function(){
      wss.send(JSON.stringify(msg));
    });
  },
  waitForSocketConnection(socket, callback){
    setTimeout(
        function () {
            if (socket.readyState === 1) {
                if(callback != null){
                    callback();
                }
                return;

            } else {
                this.waitForSocketConnection(socket, callback);
            }

        }, 5); // polling socket for readiness: 5 ms
  },

  /**************************************************************************************************************************/
  /* Sending an eval request to the server */

  /* This function is called by handleEvalRequest in the eval pane */
  sendEvalRequest(selRegion,editorState){
    console.log("In eval action creator");
    let cell = Converter.clientToASCell(selRegion,editorState);
    console.log("AS cell created for eval: " + JSON.stringify(cell));
    let msg = Converter.createEvalRequestFromASCell(cell); 
    console.log('Sending msg to server: ' + JSON.stringify(msg));
    wss.send(JSON.stringify(msg));
  },

  /**************************************************************************************************************************/
  /* Sending undo/redo messages to the server */

  sendUndoRequest(){
    let msg = Converter.createUndoRequestForServer(); 
    wss.send(JSON.stringify(msg));
  },
  sendRedoRequest(){
    let msg = Converter.createRedoRequestForServer(); 
    wss.send(JSON.stringify(msg));
  },

  /**************************************************************************************************************************/
  /* Sending get messages to the server */

  sendGetRequest(locs,vWindow) {
    let msg = Converter.clientLocsToGetMessage(locs,vWindow);
    console.log('Sending get message to server: ' + JSON.stringify(msg));
    wss.send(JSON.stringify(msg));
  },
  // TODO: NEEDS TESTING
  /* 
    Submit a get request to the server in order to get new cells when a client scrolls
    The eval store only maintains a cache based on viewing window
  */
  sendGetRequestScroll(newX, newY, oldX, oldY, vWindow) {
    console.log("Getting scrolled cells");
    let eX = Constants.scrollCacheX,
        eY = Constants.scrollCacheY,
        locs = null;
    console.log({newX, newY, oldX, oldY, vWindow});
    if (oldX === newX) {
      if (oldY < newY) { // scroll down
        locs = {col: oldX - eX + 1, row: oldY + vWindow.height + eY + 1,
                col2: oldX + vWindow.width + eX, row2: newY + vWindow.height + eY};
      } else {          // scroll up
        locs = {col: oldX - eX + 1, row: oldY - eY,
              col2: oldX + vWindow.width + eX, row2: newY - eY + 1};
      }
    } else if (oldY === newY) {
      if (oldX < newX) { // scroll right
        locs = {col: oldX + vWindow.width + eX + 1, row: oldY - eY + 1,
                col2: newX + vWindow.width + eX, row2: oldY + vWindow.height + eY};
      } else {          // scroll left
        locs = {col: oldX - eX, row: oldY - eY + 1,
                col2: newX - eX + 1, row2: oldY + vWindow.height + eY};
      }
    } else{
    }
    console.log("Unsafe scroll locs: " + JSON.stringify(locs));
    if (locs){
      let safeLocs = Converter.getSafeLoc(locs);
      this.sendGetRequest(safeLocs,vWindow);
    }
  }

};
