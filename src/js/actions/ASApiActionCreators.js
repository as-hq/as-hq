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
    case "Update":
      if (msg.result === "Success" ||
          msg.payload.tag === "PayloadC" ||
          msg.payload.tag === "PayloadCL"){
        let cells = Converter.clientCellsFromServerMessage(msg);
        Dispatcher.dispatch({
          type: ActionTypes.GOT_UPDATED_CELLS,
          updatedCells: cells
        });
      } else {
        //TODO
      }
      // TODO cases for sheets and workbooks
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
    case "Clear":
      Dispatcher.dispatch({
        type: ActionTypes.CLEARED,
      });
      break;
    // case "Delete": TODO
    //   if (msg.result === "Success")
    //     Dispatcher.dispatch({
    //       type: ActionTypes.DELETED_CELLS,
    //       locs:
    //     });
  }
};

export default {

  /**************************************************************************************************************************/
  /* Sending acknowledge message to server */

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

        }, 5)}, // polling socket for readiness: 5 ms

  sendInitialMessage(){
    let msg = Converter.makeInitMessage();
    console.log("Sending init message: " + JSON.stringify(msg));
    this.waitForSocketConnection(wss,function(){
      wss.send(JSON.stringify(msg));
    })
  },

  sendClose() {
    wss.close();
  },

  /**************************************************************************************************************************/
  /* Sending an eval request to the server */

  /* This function is called by handleEvalRequest in the eval pane */
  sendEvalRequest(selRegion,editorState){
    console.log("In eval action creator");
    let cell = Converter.clientToASCell(selRegion,editorState);
    let msg = Converter.createEvalRequestFromASCell(cell);
    console.log('Sending msg to server: ' + JSON.stringify(msg));
    wss.send(JSON.stringify(msg));
  },

  /**************************************************************************************************************************/
  /* Sending undo/redo/clear messages to the server */

  sendUndoRequest(){
    let msg = Converter.createUndoRequestForServer();
    wss.send(JSON.stringify(msg));
  },
  sendRedoRequest(){
    let msg = Converter.createRedoRequestForServer();
    wss.send(JSON.stringify(msg));
  },
  sendClearRequest(){
    let msg = Converter.createClearRequestForServer();
    wss.send(JSON.stringify(msg));
  },
  sendCopyRequest(locs) {
    let sLocs = [Converter.clientToASLocation(locs[0]), Converter.clientToASLocation(locs[1])];
    console.log(sLocs);
    let msg = Converter.toServerMessageFormat(Constants.ServerActions.Copy, "PayloadLL", sLocs);
    wss.send(JSON.stringify(msg));
  },
  sendDeleteRequest(locs){
    let msg = null;
    if (locs.constructor === Array){
      for (var i in locs)
        locs[i] = Converter.clientToASLocation(locs[i]);
      msg = Converter.toServerMessageFormat(Constants.ServerActions.Delete, "PayloadLL", locs);
    }
    else{
      locs = Converter.clientToASLocation(locs);
      msg = Converter.toServerMessageFormat(Constants.ServerActions.Delete, "PayloadLL", [locs]);
    }
    wss.send(JSON.stringify(msg));
  },

  /**************************************************************************************************************************/
  /* Sending get messages to the server */
  sendTagsMessage(action, tags, col, row) {
    let msg = Converter.toServerMessageWithPayload(action, {
      "tag": "PayloadTags",
      "tags": tags,
      "tagsLoc": Converter.clientToASLocation({col: col, row: row})
    });
    wss.send(JSON.stringify(msg));
  },
  sendGetRequest(locs) {
    let msg = Converter.clientLocsToGetMessage(locs);
    console.log('Sending get message to server: ' + JSON.stringify(msg));
    wss.send(JSON.stringify(msg));
  },

  sendOpenMessage(sheet) {
    let msg = Converter.toServerMessageFormat(Constants.ServerActions.Open, "PayloadS", sheet);
    console.log("send open message: " + JSON.stringify(msg));
    wss.send(JSON.stringify(msg));
  },

  updateViewingWindow(vWindow) {
    let sWindow = Converter.clientWindowToServer(vWindow),
        msg = Converter.toServerMessageFormat(Constants.ServerActions.UpdateWindow,
                                              "PayloadW",
                                              sWindow);
    console.log("send scroll message: " + JSON.stringify(msg));
    wss.send(JSON.stringify(msg));
  }

};
