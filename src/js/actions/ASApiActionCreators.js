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
    //xcxc
    case "GetWorkbookSheets":
      let workbooks = Converter.clientWorkbooksFromServerMessage(msg);
      Dispatcher.dispatch({
        type: ActionTypes.GOT_UPDATED_WORKBOOKS,
        workbooks: workbooks
      });
      break;
    case "Clear":
      Dispatcher.dispatch({
        type: ActionTypes.CLEARED,
      });
  }
};

export default {

  /**************************************************************************************************************************/
  /* Sending acknowledge message to server */

  waitForSocketConnection(socket, callback){
    setTimeout(() => {
      if (socket.readyState === 1) {
        if(callback != null){
            callback();
        }
        return;
      } else {
        this.waitForSocketConnection(socket, callback);
      }
    }, 5)}, // polling socket for readiness: 5 ms

  send(msg) {
    this.waitForSocketConnection(wss, () => {
      this.send(msg);
    });
  },

  sendInitialMessage(){
    let msg = Converter.makeInitMessage();
    console.log("Sending init message: " + JSON.stringify(msg));
    this.send(msg);
  },

  /**************************************************************************************************************************/
  /* Sending admin-related requests to the server */

  sendGetWorkbooks() {
    console.log("Getting workbooks");
    let msg = Converter.toServerMessageFormat('Get', 'QueryList', 'WorkbookSheets');
    this.send(msg);
  },

  /**************************************************************************************************************************/
  /* Sending an eval request to the server */

  /* This function is called by handleEvalRequest in the eval pane */
  sendEvalRequest(selRegion,editorState){
    console.log("In eval action creator");
    let cell = Converter.clientToASCell(selRegion,editorState);
    let msg = Converter.createEvalRequestFromASCell(cell);
    console.log('Sending msg to server: ' + JSON.stringify(msg));
    this.send(msg);
  },

  /**************************************************************************************************************************/
  /* Sending undo/redo/clear messages to the server */

  sendUndoRequest(){
    let msg = Converter.createUndoRequestForServer();
    this.send(msg);
  },
  sendRedoRequest(){
    let msg = Converter.createRedoRequestForServer();
    this.send(msg);
  },
  sendClearRequest(){
    let msg = Converter.createClearRequestForServer();
    this.send(msg);
  },
  sendCopyRequest(locs) {
    let msg = Converter.toServerMessageFormat(Constants.ServerActions.Copy, "PayloadLL", locs);
    this.send(msg);
  },
  sendDeleteRequest(locs){
    let msg = null;
    if (locs.constructor === Array)
      msg = Converter.toServerMessageFormat(Constants.ServerActions.Delete, "PayloadLL", locs);
    else
      msg = Converter.toServerMessageFormat(Constants.ServerActions.Delete, "PayloadL", locs);
    this.send(msg);
  },

  /**************************************************************************************************************************/
  /* Sending get messages to the server */

  sendGetRequest(locs) {
    let msg = Converter.clientLocsToGetMessage(locs);
    console.log('Sending get message to server: ' + JSON.stringify(msg));
    this.send(msg);
  },
  // TODO: NEEDS TESTING
  /*
    DEPRECATED in favor of updateViewingWindow
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
      console.log("unhandled double scroll event!");
    }
    console.log("Unsafe scroll locs: " + JSON.stringify(locs));
    if (locs){
      let safeLocs = Converter.getSafeLoc(locs);
      this.sendGetRequest(safeLocs,vWindow);
    }
  },

  updateViewingWindow(vWindow) {
    let sWindow = Converter.clientWindowToServer(vWindow),
        msg = Converter.toServerMessageFormat(Constants.ServerActions.UpdateWindow,
                                              "PayloadW",
                                              sWindow);
    console.log("send scroll message: " + JSON.stringify(msg));
    this.send(msg);
  }

};
