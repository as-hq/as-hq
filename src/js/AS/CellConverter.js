
export default {

  toASCell: function(selRegion, editorState){
    if (selRegion.width==1 && selRegion.height==1){ // not a range
      return  {
        "cellLocation": {
          "tag": "Index",
          "index": selRegion.locs[0]
        },
        "cellExpression": {
          "expression" : editorState.exp,
          "language": editorState.lang
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
          "range": selRegion.locs
        },
        "cellExpression": {
          "expression" : editorState.exp,
          "language": editorState.lang
        },
        "cellValue":{
          "tag": "ValueS",
          "contents": "initValue"
        }
      };

    }
  },

  cellToSetValueFormat: function(cell){
    return [cell.cellLocation.index[1]-1, cell.cellLocation.index[0]-1, cell.cellValue.contents]
  }




}
