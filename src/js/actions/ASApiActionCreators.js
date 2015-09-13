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
    //TODO: get response correctly
    case "Repl":
      Dispatcher.dispatch({
        type: ActionTypes.GOT_REPL_RESP,
        response:{lang:"Python",value:"44"}
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
      if (msg.payload.tag === "PayloadC" ||
          msg.payload.tag === "PayloadCL"){
        let cells = Converter.clientCellsFromServerMessage(msg);
        Dispatcher.dispatch({
          type: ActionTypes.GOT_UPDATED_CELLS,
          updatedCells: cells
        });
      } else if (msg.payload.tag === "PayloadWorkbookSheets") {
        let workbooks = Converter.clientWorkbooksFromServerMessage(msg);
        Dispatcher.dispatch({
          type: ActionTypes.GOT_UPDATED_WORKBOOKS,
          workbooks: workbooks
        });
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
    //xcxc
    case "Clear":
      Dispatcher.dispatch({
        type: ActionTypes.CLEARED,
      });
      break;
    case "EvaluateRepl":
      Dispatcher.dispatch({
        type: ActionTypes.GOT_REPL_RESP,
        response:msg.payload.contents
      });
      break;
  }
};

export default {

  /**************************************************************************************************************************/
  /* Sending acknowledge message to server */

  waitForSocketConnection(socket, callback) {
    setTimeout(() => {
      if (socket.readyState === 1) {
        if(callback != null){
          callback();
        }
        return;
      } else {
        this.waitForSocketConnection(socket, callback);
      }
    }, 5);
  }, // polling socket for readiness: 5 ms

  send(msg) {
    this.waitForSocketConnection(wss, () => {
      wss.send(JSON.stringify(msg));
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
    let msg = Converter.toServerMessageFormat('Get', 'PayloadList', 'WorkbookSheets');
    this.send(msg);
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
    let sLocs = [Converter.clientToASLocation(locs[0]), Converter.clientToASLocation(locs[1])];
    console.log(sLocs);
    let msg = Converter.toServerMessageFormat(Constants.ServerActions.Copy, "PayloadLL", sLocs);
    this.send(msg);
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
    this.send(msg);
  },

  /**************************************************************************************************************************/
  /* Sending REPL messages to the server */

  // TODO: correctly implement
  sendReplRequest(editorState){
    let msg = Converter.toServerMessageFormat(Constants.ServerActions.Repl, "PayloadXp", {
      tag: "Expression",
      expression: editorState.exp,
      language: editorState.lang
    });
    this.send(msg);
    // Dispatcher.dispatch({
    //   type: ActionTypes.GOT_REPL_RESP,
    //   response:{lang:"Python",value:"44"}
    // });
  },


  /**************************************************************************************************************************/
  /* Sending get messages to the server */
  sendTagsMessage(action, tags, col, row) {
    let msg = Converter.toServerMessageWithPayload(action, {
      "tag": "PayloadTags",
      "tags": tags,
      "tagsLoc": Converter.clientToASLocation({col: col, row: row})
    });
    this.send(msg);
  },
  sendGetRequest(locs) {
    let msg = Converter.clientLocsToGetMessage(locs);
    console.log('Sending get message to server: ' + JSON.stringify(msg));
    this.send(msg);
  },

  sendOpenMessage(sheet) {
    let msg = Converter.toServerMessageFormat(Constants.ServerActions.Open, "PayloadS", sheet);
    console.log("send open message: " + JSON.stringify(msg));
    this.send(msg);
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
