import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import CellConverter from '../AS/CellConverter';

var ActionTypes = Constants.ActionTypes;
var wss = new WebSocket(Constants.host_ws);

// when the server responds with updates to make, call dispatcher to update stores, then view
wss.onmessage = function (event) {
    console.log("client received data from server: " );
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
    let msg = CellConverter.toEvalCellsMessage(cell)
    console.log('sending msg to server: ' + JSON.stringify(msg))
    wss.send(JSON.stringify(msg));
  },

  getCells(locs) {
    let msg = CellConverter.toGetCellsMessage(locs);
    console.log('sending msg to server: ' + JSON.stringify(msg))
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
      // if (oldX < newX && oldY < newY )
      //   locs = [{col: newX + vWindow.width, row: newY - eY,
      //            col2: newX + vWindow.width + eX, row2: newY + vWindow.height + eY},
      //           {col: newX - eX, row: newY + vWindow.height,
      //            col2: newX + vWindow.width, row2: newY + vWindow.height + eY}];
      // else if (oldX < newX && oldY > newY)
      //   locs = [{col: newX + vWindow.width, row: newY - eY,
      //            col2: newX + vWindow.width + eX, row2: newY + vWindow.height + eY},
      //           {col: newX - eX, row: newY + vWindow.height,
      //            col2: newX + vWindow.width, row2: newY + vWindow.height + eY}];
    }
    console.log("unsafe scroll locs: " + JSON.stringify(locs));
    if (locs){
      let safeLocs = CellConverter.getSafeLoc(locs);
      this.getCells(safeLocs);
    }
  }

};
