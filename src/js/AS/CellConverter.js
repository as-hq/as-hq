import Constants from '../Constants';

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

  toASCell(selRegion, editorState){
    console.log(editorState.lang)
    if (selRegion.width==1 && selRegion.height==1){ // not a range
      return  {
        "cellLocation": {
          "tag": "Index",
          "sheet": "Demo",
          "index": this.standardToServerLoc(selRegion.locs[0])
        },
        "cellExpression": {
          "tag": "Expression",
          "expression" : editorState.exp,
          "language": editorState.lang.Server
        },
        "cellValue":{
          "tag": "ValueS",
          "contents": "initValue"
        }
      };
    }
    else {
      return {
        "cellLocation": {
          "tag": "Range",
          "sheet": "Demo",
          "range": selRegion.locs.map(this.standardToServerLoc)
        },
        "cellExpression": {
          "tag": "Expression",
          "expression" : editorState.exp,
          "language": editorState.lang.Server
        },
        "cellValue":{
          "tag": "ValueS",
          "contents": "initValue"
        }
      };

    }
  },

  toServerMessageFormat(action, payloadTag, payload) {
    return {
      "action": action,
      "payload": {
        "tag": payloadTag,
        "contents": payload
      },
      "result": "Failure"  //failure by default until server sets success
    };
  },

  toEvalCellsMessage(cell) {
    return this.toServerMessageFormat(Constants.ServerActions.Evaluate, "PayloadC", cell);
  },

  cellToGridValue(cell) {
    return {
      index: cell.cellLocation.index,
      display: cell.cellValue.contents
    };
  },

  getCellsFromMsg(msg) {
    console.log("message received: " + JSON.stringify(msg));
    if (msg.payload){
      if (msg.payload === 'ACK')
        console.log("SERVER ACKNOWLEDGES");
      else if (msg.payload.tag === "PayloadC"){
        console.log("single cell");
        return [this.fromServerCell(msg.payload.contents)];
      }
      else if (msg.payload.tag === "PayloadCL"){
        console.log("multi cell");
        return msg.payload.contents.map(this.fromServerCell);
      }
    }
    else
      console.log("error parsing: no payload found in message");
  },

  toGetCellsMessage(locs) {
    let tag = null;
    if (locs.row2)
      tag = "PayloadLL";
    else
      tag = "PayloadL";
    let sLocs = this.standardToServerLoc(locs);
    return this.toServerMessageFormat(Constants.ServerActions.Get, tag, sLocs);
  }
}
