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

  simpleToASRange(rng) {
    let asRange = {tag: 'range', range: rng};
    return this.addCurrentSheetIdToObj(asRange);
  },

  simpleToASIndex(idx) {
    let asIndex = {tag: 'index', index: idx};
    return this.addCurrentSheetIdToObj(asIndex);
  },

  indexToRange(ind) { 
    return { tl: ind, br: ind };
  },

  indexToSelection(ind) { 
    return { origin: ind, range: { tl: ind, br: ind } };
  },

  rangeToASWindow(rng) {
    return this.addCurrentSheetIdToObj({ window: rng });
  },

  rangeToASIndices(rng) {
    let inds = [];
    for (var r = rng.tl.row; r <= rng.br.row; r++){
      for (var c = rng.tl.col; c <= rng.br.col; c++) {
        inds.push(this.simpleToASIndex({row: r, col: c}));
      }
    }
    return inds;
  },

  asLocationToSimple(loc) {
    return (loc.tag === 'index') ? {tl: loc.index, br: loc.index} : loc.range;
  },

  asSelectionToSimple(sel) { 
    return {range: sel.selectionRange.range, origin: sel.selectionOrigin.index};
  },

  /**************************************************************************************************************************/
  /* Type conversion utils */

  addCurrentSheetIdToObj(obj) {
    obj.sheetId = Store.getCurrentSheet().sheetId;
    return obj;
  }

}
