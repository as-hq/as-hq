import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';
import T from '../AS/Types';

export default {


  /**************************************************************************************************************************/
  /* Type constructors */

  makeClientMessage(action, payloadTag, payloadContents) {
    return this.makeClientMessageRaw(action, { "tag": payloadTag,
                                               "contents": payloadContents });
  },

  makeClientMessageRaw(action, payload) {
    return { "action": action, "payload": payload };
  },

  makeEvalCell(asIndex, xpObj) {
    return  {
      "cellLocation": asIndex,
      "cellExpression": {
        "tag": "Expression",
        "expression": xpObj.expression,
        "language": xpObj.language.Server
      },
      "cellValue":{
        "tag": "NoValue",
        "contents": []
      },
      "cellTags": []
    };
  },

  makeEmptyCell(asIndex){
    let cl = asIndex || {tag:"index",
              sheetId: "TEST_SHEET_ID",
              index:{row: -1, col:-1}},
        ce = {tag:"Expression",expression:"",language:null},
        cv = {tag:"NoValue", contents: []},
        ct = [];
    return {cellLocation:cl,cellExpression:ce,cellValue:cv,cellTags:ct};
  },

  makeASIndex(sheetId, col, row) {
    return {
      sheetId: sheetId,
      tag: 'index',
      index: {row: row, col: col}
    };
  },

  makeWorkbookSheet() {
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

  makeWorkbook() {
    return {
      workbookName: "",
      workbookSheets: []
    };
  },

  makeSheet() {
    return {
      sheetId: "",
      sheetName: sheetName,
      sheetPermissions: {
        tag: 'Blacklist',
        contents: []
      }
    };
  },


  /**************************************************************************************************************************/
  /* Type conversions */

  simpleToASLocation(loc) {
    return T.isIndex(loc) ? this.simpleToASIndex(loc) : this.simpleToASRange(loc);
  },

  simpleToASRange(rng) {
    let asRange = {tag: 'range', range: rng};
    return this.addCurrentSheetIdToObj(asRange);
  },

  simpleToASIndex(rng) {
    let asIndex = {tag: 'index', index: rng.tl};
    return this.addCurrentSheetIdToObj(asIndex);
  },

  rangeToASWindow(rng) {
    return this.addCurrentSheetIdToObj({ window: rng });
  },

  ASLocationToSimple(loc) {
    return (loc.tag === 'index') ? {tl: loc.index, br: loc.index} : loc.range;
  },


  /**************************************************************************************************************************/
  /* Type conversion utils */

  addCurrentSheetIdToObj(obj) {
    obj.sheetId = Store.getCurrentSheet().sheetId;
    return obj;
  }

}
