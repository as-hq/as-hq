import Constants from '../Constants';
import ASEvaluationStore from '../stores/ASEvaluationStore';

function getSafeLoc(loc) {
  console.log(loc);
  if (loc.length)
    return loc.map(getSafeLoc);
  else {
    let safeLoc = {
      row: Math.min(Math.max(0, loc.row), Constants.numRows),
      col: Math.min(Math.max(0, loc.col), Constants.numCols)
    };
    if (loc.row2){
      safeLoc.row2 = Math.min(Math.max(0, loc.row2), Constants.numRows);
      safeLoc.col2 = Math.min(Math.max(0, loc.col2), Constants.numCols);
    }
    return safeLoc;
  }
};

function serverToStandardLoc(loc) {
  // console.log(loc);
  if (loc[0].length)
    return {row: loc[0][1], col: loc[0][0], row2: loc[1][1], col2: loc[1][0]};
  else return {row: loc[1], col: loc[0]};
};

export default {

  getSafeLoc: getSafeLoc,

  serverToStandardLoc: serverToStandardLoc,

  standardToServerLoc(loc) {
    if (loc.row2)
      return [[loc.col, loc.row], [loc.col2, loc.row2]];
    else return [loc.col, loc.row];
  },

  fromServerCell(cell) {
    console.log(cell.cellLocation.index);
    return {
      cellLocation: {
        tag: cell.cellLocation.tag,
        sheet: cell.cellLocation.sheet,
        index: serverToStandardLoc(cell.cellLocation.index)
      },
      cellExpression: cell.cellExpression,
      cellValue: cell.cellValue
    };
  },

  toASLoc(loc) {
    if (loc.row2)
      return {tag: "Range",
              sheet: ASEvaluationStore.getSheet(),
              range: this.standardToServerLoc(loc)};
    else
      return {tag: "Index",
              sheet: ASEvaluationStore.getSheet(),
              index: this.standardToServerLoc(loc)};
  },

  toASCell(selRegion, editorState){
    console.log(editorState.lang)
    return  {
      "cellLocation": this.toASLoc(selRegion.range),
      "cellExpression": {
        "tag": "Expression",
        "expression" : editorState.exp,
        "language": editorState.lang.Server
      },
      "cellValue":{
        "tag": "ValueS",
        "contents": "initValue"
      }};
  },

  toServerMessageFormat(action, payloadTag, payload) {
    return {
      "action": action,
      "payload": {
        "tag": payloadTag,
        "contents": payload
      },
      "result": {"tag":"NoResult","contents":[]}  // by default until server sets success
    };
  },


  cellToGridValue(cell) {
    return {
      index: cell.cellLocation.index,
      display: cell.cellValue.contents
    };
  },

  getCellsFromMsg(msg) {
    console.log("Trying to get cells from message received: " + JSON.stringify(msg));
    if (msg.payload){
      if (msg.payload === 'ACK')
        console.log("SERVER ACKNOWLEDGES");
      else if (msg.payload.tag === "PayloadC"){
        return [this.fromServerCell(msg.payload.contents)];
      }
      else if (msg.payload.tag === "PayloadCL"){
        console.log("Number of eval cells received: " + msg.payload.contents.length);
        let cells = msg.payload.contents.map(this.fromServerCell);
        console.log("Eval cells JSON: " + JSON.stringify(cells)); 
        return cells; 
      }
    }
    else
      console.log("Error parsing: no payload found in message");
  },

  toGetCellsMessage(locs) {
    let tag = null,
        sLocs = null;
    if (locs.length){
      tag = "PayloadLL";
      sLocs = locs.map(this.toASLoc);
    }
    else{
      tag = "PayloadL";
      sLocs = this.toASLoc(locs);
    }
    return this.toServerMessageFormat(Constants.ServerActions.Get, tag, sLocs);
  }

}
