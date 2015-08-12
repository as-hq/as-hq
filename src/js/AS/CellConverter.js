import Constants from '../Constants';

export default {

  toASCell(selRegion, editorState){
    console.log(editorState.lang)
    if (selRegion.width==1 && selRegion.height==1){ // not a range
      return  {
        "cellLocation": {
          "tag": "Index",
          "sheet": "Demo",
          "index": selRegion.locs[0]
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
          "range": selRegion.locs
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

  toServerEvalFormat(cell) {
    return this.toServerMessageFormat(Constants.ServerActions.Evaluate, "PayloadC", cell);
  },

  cellToSetValueFormat(cell){
    return [cell.cellLocation.index[1]-1, cell.cellLocation.index[0]-1, cell.cellValue.contents]
  },

  getCellsFromMsg(msg) {
    console.log("message received: " + JSON.stringify(msg));
    if (msg.payload){
      if (msg.payload === 'ACK')
        console.log("SERVER ACKNOWLEDGES");
      else
        return msg.payload;
    }
    else
      console.log("error parsing: no payload found in message");
  }


}
