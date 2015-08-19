import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import CellConverter from '../AS/CellConverter';

var ActionTypes = Constants.ActionTypes;
var wss = new WebSocket(Constants.host_ws);

// Called whenever the websocket server returns a message
wss.onmessage = function (event) {
    console.log("Client received data from server: " + JSON.stringify(event.data));
    let msg = JSON.parse(event.data);
    if (msg.action === "Acknowledge")
      return;
    if (msg.action === "Undo"){
      Dispatcher.dispatch({
        type: ActionTypes.GOT_UNDO,
        commit: msg.payload.contents
      });
      return;
    }
    if (msg.action === "Redo"){
     Dispatcher.dispatch({
        type: ActionTypes.GOT_REDO,
        commit: msg.payload.contents
      });
     return;
    }
    let cells = CellConverter.getCellsFromMsg(msg);
    if (cells){
      Dispatcher.dispatch({
        type: ActionTypes.GOT_UPDATED_CELLS,
        updatedCells: cells
      });
      return;
    }
};

export default {

  sendInitialMessage(userName){
    let msg = CellConverter.toServerMessageFormat(Constants.ServerActions.Acknowledge,"PayloadInit",{userName:userName});
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

        }, 5);
  },

  // this function submits an eval request to a websocket server
  sendEvalRequest(selRegion,editorState, vw){
    console.log("In eval action creator");
    let cell = CellConverter.toASCell(selRegion,editorState);
    let vwContents = {vwTopLeftCol:vw.locs[0][0], vwTopLeftRow: vw.locs[0][1], vwWidth:vw.width, vwHeight:vw.height};
    let payload = {tag:"PayloadC", evalCell:cell, evalVW: vwContents}
    let msg = {action:"Evaluate",result:{"tag":"NoResult","contents":[]},payload:payload};
    console.log('Sending msg to server: ' + JSON.stringify(msg));
    wss.send(JSON.stringify(msg));
  },

  sendUndoRequest(){
    let msg = {action:"Undo",result:{"tag":"NoResult","contents":[]},payload:{tag:"PayloadN", contents:[]}};
    wss.send(JSON.stringify(msg));
  },

  sendRedoRequest(){
    let msg = {action:"Redo",result:{"tag":"NoResult","contents":[]},payload:{tag:"PayloadN", contents:[]}};
    wss.send(JSON.stringify(msg));
  },

  getCells(locs) {
    let msg = CellConverter.toGetCellsMessage(locs);
    console.log('sending msg to server: ' + JSON.stringify(msg));
    wss.send(JSON.stringify(msg));
  },

  getCellsForScroll(newX, newY, oldX, oldY, vWindow) {
    // TODO test
    console.log("getting scrolled cells");
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
    console.log("unsafe scroll locs: " + JSON.stringify(locs));
    if (locs){
      let safeLocs = CellConverter.getSafeLoc(locs);
      this.getCells(safeLocs);
    }
  }

};
