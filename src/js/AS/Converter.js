import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';
import T from '../AS/Types';

export default {

  /**************************************************************************************************************************/
  /* Convenience methods for object manipulation */

  addCurrentSheetIdToObj(obj) {
    obj.sheetId = Store.getCurrentSheet().sheetId;
    return obj;
  },


  /**************************************************************************************************************************/
  /* Convenience methods for object creation */

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

  makeEmptyCell(loc){
    let cl = loc || {tag:"index",
              sheetId: "TEST_SHEET_ID",
              index:{row: -1, col:-1}},
        ce = {tag:"Expression",expression:"",language:null},
        cv = {tag:"NoValue", contents: []},
        ct = [];
    return {cellLocation:cl,cellExpression:ce,cellValue:cv,cellTags:ct};
  },

  makeIndex(sheetId, col, row) {
    return {
      sheetId: sheetId,
      tag: 'index',
      index: {row: row, col: col}
    };
  },


/**************************************************************************************************************************/
  /* Workbooks and sheets */
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


  /**************************************************************************************************************************/
  /* Type conversions */

  simpleToASLocation(loc) {
    let tag = T.isIndex(loc) ? 'index' : 'range',
        asLoc = {tag: tag};
    asLoc[tag] = loc;
    return this.addCurrentSheetIdToObj(asLoc);
  },

  simpleToASRange(rng) {
    let asRange = {tag: 'range', range: rng};
    return this.addCurrentSheetIdToObj(asRange);
  },

  simpleToASIndex(idx) {
    let asIndex = {tag: 'index', index: idx};
    return this.addCurrentSheetIdToObj(asIndex);
  },

  ASLocationToSimple(loc) {
    return (loc.tag === 'index') ? {tl: loc.index, br: loc.index} : loc.range;
  },

  rangeToASWindow(rng) {
    return this.addCurrentSheetIdToObj({ window: rng });
  },

  /**************************************************************************************************************************/
  /* Message creation */

  createSheetRequest(sheetName) {
    return this.toServerMessageFormat(Constants.ServerActions.New, "PayloadS", {
      sheetId: "",
      sheetName: sheetName,
      sheetPermissions: {
        tag: 'Blacklist',
        contents: []
      }
    });
  },

/**************************************************************************************************************************/
  /* External conversions */

  externalStringToExpression(str, lang) {
    if (lang.Server == "Excel") { // is language.Editor the correct thing?
      return str;
    } else if (str != null && typeof(str) != "undefined") {
      if (!isNaN(Number(str))) {
        return str;
      } else if (str.toUpperCase() == "TRUE") {
        return this.externalStringToBool(true, lang.Server);
      } else if (str.toUpperCase() == "FALSE") {
        return this.externalStringToBool(false, lang.Server);
      } else {
        return JSON.stringify(str);
      }
    } else {
      return ""; // unclear if we ever get here -- Alex 10/19
    }
  },

  externalStringToBool(b, lang) {
    if (b) {
      if (["R", "OCaml"].indexOf(lang) != -1) {
        return "true";
      } else {
        return "True";
      }
    } else {
      if (["R", "OCaml"].indexOf(lang) != -1) {
        return "false";
      } else {
        return "False";
      }
    }
    throw "Should never make it to the end of _dispBoolInLang";
  }

}
