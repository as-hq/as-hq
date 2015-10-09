import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util.js'

export default {

  clientWorkbooksFromServerMessage(msg) {
    let {payload} = msg;
    let {contents, tag} = payload;

    if (tag === 'PayloadWorkbookSheets') {
      return contents.reduce((acc, wb) => {
        acc[wb.wsName] = wb;
        return acc;
      }, {});
    } else {
      throw new Error('The payload is not workbook sheets.');
    }
  },

  /**************************************************************************************************************************/
  /*
    Methods so that stores/components doesn't have to worry about conversions dependent on the type system/object architecture
    Here, clientCell (the type stored in eval store) is compliant with the current clientCell standard:
    cellLocation: {
        tag:
        sheet:
        index: {col,row} (the only cells that will be updated are Index cells)
      },
      cellExpression: {
        "tag":
        "expression":
        "language": (from Constants)
      }
      cellValue: {
        "tag": ("ValueS", etc.)
        "contents":
      }
  */
  clientCellGetSheetId(clientCell){
    return clientCell.cellLocation.locSheetId;
  },
  clientCellGetCol(clientCell){
    return clientCell.cellLocation.index.col;
  },
  clientCellGetRow(clientCell){
    return clientCell.cellLocation.index.row;
  },
  clientCellGetExpressionObj(clientCell){
    return clientCell.cellExpression;
  },
  clientCellGetValueObj(clientCell){
    return clientCell.cellValue;
  },
  clientCellEmpty(loc){
    let ce = {"tag":"Expression","expression":"","language":"Python"};
    let cv = {"tag": "NoValue", "contents": []};
    return {cellLocation:loc,cellExpression:ce,cellValue:cv,cellTags:[]};
  },
  /* Convert a store cell to a grid display for hypergrid's format */
  clientCellGetDisplay(clientCell) {
    return Util.showValue(clientCell.cellValue);
  },

  /**************************************************************************************************************************/
  /*
    Location (excluding the tag) conversion and methods
    Server location: [c,r] or [[c,r],[c,r]]
    Client location: {row,col} {row,col,row2,col2}
  */

  serverToClientLoc(serverLoc){
    // console.log("converting server loc: " + JSON.stringify(serverLoc));
    if (serverLoc[0].length) {// range
      if (serverLoc[0][0] === serverLoc [1][0] && serverLoc[0][1] === serverLoc[1][1])
        return {row: serverLoc[0][1], col: serverLoc[0][0]};
      else
        return {row: serverLoc[0][1], col: serverLoc[0][0], row2: serverLoc[1][1], col2: serverLoc[1][0]};
    }
    else return {row: serverLoc[1], col: serverLoc[0]};
  },
  clientToServerLoc(clientLoc) {
    if (clientLoc.row2)
      return [[clientLoc.col, clientLoc.row], [clientLoc.col2, clientLoc.row2]];
    else return [clientLoc.col, clientLoc.row];
  },
  clientToASLocation(clientLoc) {
    if (clientLoc.row2)
      return {tag: "Range",
              locSheetId: Store.getCurrentSheet().sheetId,
              range: this.clientToServerLoc(clientLoc)};
    else
      return {tag: "Index",
              locSheetId: Store.getCurrentSheet().sheetId,
              index: this.clientToServerLoc(clientLoc)};
  },
  clientToASRange(clientLoc) {
    if (clientLoc.row2)
      return {tag: "Range",
              rangeSheetId: Store.getCurrentSheet().sheetId,
              range: this.clientToServerLoc(clientLoc)};
    else
      return {tag: "Range",
              rangeSheetId: Store.getCurrentSheet().sheetId,
              range:  [this.clientToServerLoc(clientLoc), this.clientToServerLoc(clientLoc)]};
  },
  /*
    Returns a location (index or range) if its inside the sheet boundaries; otherwise one of the corners of the sheet boundaries
    Works on an array of locations, uses the sheet format
  */
  getSafeLoc(loc) {
    // console.log("Location in get safe loc: " + loc);
    if (loc.length)
      return loc.map(this.getSafeLoc);
    else {
      let safeLoc = {
        row: Math.min(Math.max(1, loc.row), Constants.numRows),
        col: Math.min(Math.max(1, loc.col), Constants.numCols)
      };
      if (loc.row2){
        safeLoc.row2 = Math.min(Math.max(1, loc.row2), Constants.numRows);
        safeLoc.col2 = Math.min(Math.max(1, loc.col2), Constants.numCols);
      }
      return safeLoc;
    }
  },

  clientWindowToServer(vWindow) {
    return {
      windowSheetId: Store.getCurrentSheet().sheetId,
      topLeft: [vWindow.range.col, vWindow.range.row],
      bottomRight: [vWindow.range.col2, vWindow.range.row2]
    };
  },

  /**************************************************************************************************************************/
  /*
    Cell conversion and processing
    Server cell standard (client cell standard is above)
      "cellLocation": serverLoc (see above),
      "cellExpression": {
        "tag": "Expression",
        "expression" :
        "language":
      },
      "cellValue":{
        "tag": "NoValue",
        "contents": []
      },
      "cellTags": []};
  */

  ASLocToClient(loc) {
    console.log("converting loc: "+JSON.stringify(loc));
    let cloc = {
        locSheetId: loc.locSheetId || loc.rangeSheetId
      };
    if (loc.range){
      let rng = this.serverToClientLoc(loc.range)
      if (rng.row2)
        cloc.range = rng;
      else
        cloc.index = rng;
    }
    else
      cloc.index = this.serverToClientLoc(loc.index);
    return cloc;
  },

  serverToClientCell(serverCell) {
    let serverLoc = serverCell.cellLocation.index;
    // console.log("Server cell: " + serverLoc);
    return {
      cellLocation: {
        tag: serverCell.cellLocation.tag,
        locSheetId: serverCell.cellLocation.locSheetId,
        index: this.serverToClientLoc(serverLoc)
      },
      cellExpression: serverCell.cellExpression,
      cellValue: serverCell.cellValue,
      cellTags: serverCell.cellTags
    };
  },
  /*
    Used to create an initial ASCell to send to server for eval
    Selection region has width, height, and range, where range is in the client loc format
  */
  clientToASCell(selRegion, editorState){
    // console.log("making cell with language: " + JSON.stringify(editorState.lang));
    return  {
      "cellLocation": this.clientToASLocation(selRegion.range),
      "cellExpression": {
        "tag": "Expression",
        "expression" : editorState.exp,
        "language": editorState.lang.Server
      },
      "cellValue":{
        "tag": "NoValue",
        "contents": []
      },
      "cellTags": []      // TODO pass in tags as arguments
    };
  },
  defaultCell(){
    let cl = {tag:"Index",
              locSheetId: "TEST_SHEET_ID",
              index:[-1,-1]},
        ce = {tag:"Expression",expression:"",language:null},
        cv = {tag:"NoValue", contents: []},
        ct = [];
    return {cellLocation:cl,cellExpression:ce,cellValue:cv,cellTags:ct};
  },

  /**************************************************************************************************************************/
  /* Commit (undo/redo) conversion */

  /* Commits have list of ASCells which need to get converted to client type cells */
  serverToClientCommit(commit){
    // console.log("Converting commit to client format: " + JSON.stringify(commit));
    let before = []; let after = [];
    for (var key in commit.before){
      let clientCell = this.serverToClientCell(commit.before[key]);
      before.push(clientCell);
    }
    for (var key in commit.after){
      let clientCell = this.serverToClientCell(commit.after[key]);
      after.push(clientCell);
    }
    return {user:commit.user,before:before,after:after,time:commit.time};
  },

  /**************************************************************************************************************************/
  /* Message conversion */

  toServerMessageFormat(action, payloadTag, payload) {
    return {
      "messageUserId": Store.getUserId(),
      "action": action,
      "payload": {
        "tag": payloadTag,
        "contents": payload
      },
      "result": {"tag":"NoResult","contents":[]}  // by default until server sets success
    };
  },
  toServerMessageWithPayload(action, payload) {
    return {
      "messageUserId": Store.getUserId(),
      "action": action,
      "payload": payload,
      "result": {"tag":"NoResult","contents":[]}  // by default until server sets success
    };
  },
  /* If a server message has cells, they are in server cell format. Convert to client cell format */
  clientCellsFromServerMessage(msg) {
    // console.log("Trying to get cells from message received: " + JSON.stringify(msg));
    if (msg.payload){
      if (msg.payload === 'ACK')
        console.log("SERVER ACKNOWLEDGES");
      else if (msg.payload.tag === "PayloadC"){ // one cell
        return [this.serverToClientCell(msg.payload.contents)];
      }
      else if (msg.payload.tag === "PayloadCL"){ // list of cells
        // console.log("Number of eval cells received: " + msg.payload.contents.length);
        let cells = [];
        for (var key in msg.payload.contents){
          let clientCell = this.serverToClientCell(msg.payload.contents[key]);
          cells.push(clientCell);
        }
        // console.log("Eval cells JSON: " + JSON.stringify(cells));
        return cells;
      }
    }
    else
      console.log("Error parsing: no payload found in message");
  },
  clientLocsFromServerMessage(msg) {
    // console.log("Trying to get cells from message received: " + JSON.stringify(msg));
    if (msg.payload){
      if (msg.payload.tag === "PayloadL" || msg.payload.tag === "PayloadR"){ // one cell
        return [this.ASLocToClient(msg.payload.contents)];
      }
      else if (msg.payload.tag === "PayloadLL"){ // list of cells
        let locs = [];
        for (var key in msg.payload.contents){
          let clientLoc = this.ASLocToClient(msg.payload.contents[key]);
          locs.push(clientLoc);
        }
        // console.log(locs);
        return locs;
      }
    }
    else
      console.log("Error parsing: no payload found in message");
  },
  /*
    Used to make a Get request to the server for the cells at certain locations
    For example, when the store needs to update due to a scroll, query server
  */
  clientLocsToGetMessage(locs) {
    let tag = null,
        sLocs = null;
    if (locs.length){
      tag = "PayloadLL";
      sLocs = [];
      for (var key in locs){
        sLocs.push(this.clientToASLocation(locs[key]));
      }
    }
    else{
      console.log("Not going to handle only getting one cell");
    }
    return this.toServerMessageFormat(Constants.ServerActions.Get, tag, sLocs);
  },

/**************************************************************************************************************************/
  /* Workbooks and sheets */
  newWorkbookSheet() {
    return {
      wsName: "",
      wsSheets: [{
        sheetId: "",
        sheetName: "",
        sheetPermissions:{
          tag: "Blacklist",
          contents: []
        }
      }]
    };
  },

  newWorkbook() {
    return {
      workbookName: "",
      workbookSheets: []
    };
  },

  /**************************************************************************************************************************/
  /* Message creation */

  makeInitMessage() {
    return this.toServerMessageFormat(Constants.ServerActions.Acknowledge, "PayloadInit", {"connUserId": Store.getUserId()});
  },
  /* Used to create undo/redo messages to submit to server. Called by send Undo/Redo Request in API action creator */
  createUndoRequestForServer(){
    return this.toServerMessageFormat(Constants.ServerActions.Undo, "PayloadN", []);
  },
  createRedoRequestForServer(){
    return this.toServerMessageFormat(Constants.ServerActions.Redo, "PayloadN", []);
  },
  /* Create an evaluation request in the message format. Called by sendEvalRequest in API action creator */
  createEvalRequestFromASCell(asCell){
    return this.toServerMessageFormat(Constants.ServerActions.Evaluate, "PayloadC", asCell)
  },
  createClearRequestForServer(){
    return this.toServerMessageFormat(Constants.ServerActions.Clear, "PayloadN", []);
  },

  createSheetRequest(sheetName) {
    return this.toServerMessageFormat(Constants.ServerActions.New, "PayloadS", {
      sheetId: "",
      sheetName: sheetName,
      sheetPermissions: {
        tag: 'Blacklist',
        contents: []
      }
    });
  }

}
