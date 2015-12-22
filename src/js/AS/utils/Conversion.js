/* @flow */

import type {
  ASAction
} from '../../types/Actions';

import type {
  PairObject
} from '../../types/Base';

import type {
  NakedIndex,
  NakedRange,
  ASSelection,
  ASIndex,
  ASRange,
  ASLocation,
  ASSheet,
  ASWorkbook,
  ASCell
} from '../../types/Eval';

import type {
  ASViewingWindow,
  ASClientExpression
} from '../../types/State';

import type {
  PayloadSelection,
  ASBackendWorkbookSheet,
  ASClientWindow,
  ASMessageAction,
  ASClientMessage
} from '../../types/Messages';

import Location from './Location';

import Constants from '../../Constants';
import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';

let CU = {
  /**************************************************************************************************************************/
  /* Type constructors */

  makeClientMessage(
    action: ASMessageAction,
    payloadTag: string,
    payloadContents: any
  ): ASClientMessage {
    return CU.makeClientMessageRaw(action, { "tag": payloadTag,
                                               "contents": payloadContents });
  },

  makeClientMessageRaw(
    action: ASMessageAction,
    payload: any
  ): ASClientMessage {
    return { "action": action, "payload": payload };
  },

  makeEvalCell(asIndex: ASIndex, xpObj: ASClientExpression): ASCell {
    return  {
      "cellLocation": asIndex,
      "cellExpression": {
        "tag": "Expression",
        "expression": xpObj.expression,
        "language": xpObj.language
      },
      "cellValue":{
        "tag": "NoValue",
        "contents": []
      },
      "cellProps": []
    };
  },

  makeEmptyCell(asIndex?: ASIndex): ASCell {
    let cl = asIndex || {tag:"index",
              sheetId: "TEST_SHEET_ID",
              index:{row: -1, col:-1}},
        ce = {tag:"Expression",expression:"",language:null},
        cv = {tag:"NoValue", contents: []},
        cp = [];
    return {cellLocation:cl, cellExpression:ce, cellValue:cv, cellProps:cp};
  },

  makeASIndex(sheetId: string, col: number, row: number): ASIndex {
    return {
      sheetId: sheetId,
      tag: 'index',
      index: {row: row, col: col}
    };
  },

  makeWorkbookSheet(): ASBackendWorkbookSheet {
    return {
      tag: 'WorkbookSheet',
      wsName: "",
      wsSheets: [{
        tag: 'Sheet',
        sheetId: "",
        sheetName: "",
        sheetPermissions:{
          tag: "Blacklist",
          contents: []
        }
      }]
    };
  },

  makeWorkbook(): ASWorkbook {
    return {
      tag: 'Workbook',
      workbookName: "",
      workbookSheets: []
    };
  },

  makeSheet(sheetName: string): ASSheet {
    return {
      tag: 'Sheet',
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

  intToChar(i: number): string {
    return 'ABCDEFGHIJKLMNOPQRSTUVXYZ'.charAt(i);
  },

  charToInt(c: string): number {
    return c.charCodeAt(0) - 64;
  },

  simpleToASRange(rng: NakedRange, sheetId?: string): ASRange {
    if (typeof(sheetId) == "undefined") sheetId = SheetStateStore.getCurrentSheet().sheetId;
    return {tag: 'range', range: rng, sheetId: sheetId};
  },

  simpleToASIndex(idx: NakedIndex, sheetId?: string): ASIndex {
    if (typeof(sheetId) == "undefined") sheetId = SheetStateStore.getCurrentSheet().sheetId;
    return {tag: 'index', index: idx, sheetId: sheetId};
  },

  intToExcelCol(i: number): string {
    i = i -1;
    var quo = Math.floor((i) / 26);
    var rem = (i) % 26;
    var code = '';
    if (quo > 0) {
        code += String.fromCharCode('A'.charCodeAt(0) + quo - 1);
    }
    code += String.fromCharCode('A'.charCodeAt(0) + rem);
    return code;
  },

  //xcxc: I know it's bad that this is here, but I couldn't get it to work in Util.js.
  //  Apparently JS is really bad at circular dependency injection and it was fucking up
  //  getHostUrl()
  excelToASRange(xp: string): ASRange {
    let parts = xp.split('!');

    if (parts.length === 1) {
      let [nakedExcel] = parts;
      return CU.simpleToASRange(CU.excelToRange(nakedExcel));
    } else if (parts.length === 2) {
      let [sheetId, nakedExcel] = parts;
      return CU.simpleToASRange(CU.excelToRange(nakedExcel), sheetId);
    } else {
      throw new Error('Does not support workbooks yet');
    }
  }, 

  indexToRange(ind: NakedIndex): NakedRange {
    return { tl: ind, br: ind };
  },

  indexToSelection(ind: NakedIndex): ASSelection {
    return { origin: ind, range: { tl: ind, br: ind } };
  },

  rangeToASWindow(rng: NakedRange): ASClientWindow {
    let sheetId = SheetStateStore.getCurrentSheet().sheetId;
    return { window: rng, sheetId: sheetId };
  },

  rangeToASIndices(rng: NakedRange): Array<ASIndex> {
    let inds = [];
    for (var r = rng.tl.row; r <= rng.br.row; r++) {
      for (var c = rng.tl.col; c <= rng.br.col; c++) {
        inds.push(CU.simpleToASIndex({row: r, col: c}));
      }
    }
    return inds;
  },

  rangeToIndices(rng: NakedRange): Array<NakedIndex> {
    let inds = [];
    for (var r = rng.tl.row; r <= rng.br.row; r++) {
      for (var c = rng.tl.col; c <= rng.br.col; c++) {
        inds.push({row: r, col: c});
      }
    }
    return inds;
  },

  asRangeToASIndices(rng: ASRange): Array<ASIndex> {
    let inds = [], 
        {range, sheetId} = rng;
    for (var r = range.tl.row; r <= range.br.row; r++) {
      for (var c = range.tl.col; c <= range.br.col; c++) {
        inds.push(this.simpleToASIndex({row: r, col: c}, sheetId));
      }
    }
    return inds;
  },

  asLocsToASIndices(locs: Array<ASLocation>): Array<ASIndex> { 
    let indicesList = locs.map((l) => { 
      switch (l.tag) { 
        case 'range': return this.asRangeToASIndices(l); 
        case 'index': return [l]; 
        default: throw "tag invalid in a location passed into asLocsToASIndices"; 
      }
    });

    return [].concat.apply([], indicesList);
  },

  excelToIndex(dollarRef: string): NakedIndex {
    let ref = dollarRef.replace(/\$/g, '').toUpperCase();
    var row=0, col=0, i=0, charIdx = 0;
    while (i < ref.length && isNaN(ref.charAt(i))) {
      charIdx = i+1;
      i++;
    }
    var rawCol = ref.substring(0, charIdx), rawRow = ref.substring(charIdx);
    for (var c=0; c<charIdx; c++) {
      col = col + CU.charToInt(ref.charAt(c)) * Math.pow(26, charIdx - c-1);
    }

    if (rawRow.length > 0) {
      return {col: col, row: parseInt(rawRow)};
    } else {
      return {col: col, row: Infinity};
    }
  },

  excelToRange(xp: string): NakedRange {
    let endpoints = xp.split(":");
    if (endpoints.length === 1) {
      let idx = CU.excelToIndex(endpoints[0]);
      return { tl: idx, br: idx };
    } else {
      let start = CU.excelToIndex(endpoints[0]),
          end = CU.excelToIndex(endpoints[1]);
      return { tl: start, br: end };
    }
  },

  rangeToExcel(rng: NakedRange): string {
    if (Location.isIndex(rng)) {
      return CU.intToExcelCol(rng.tl.col) + rng.tl.row;
    } else {
      let {tl, br} = Location.orientRange(rng);
      return CU.intToExcelCol(tl.col) + tl.row
        + ":"
        + CU.intToExcelCol(br.col) + br.row;
    }
  },

  asLocationToSimple(loc: ASLocation): NakedRange {
    return (loc.tag === 'index') ? {tl: loc.index, br: loc.index} : loc.range;
  },

  asSelectionToSimple(sel: PayloadSelection): ASSelection {
    return {range: sel.selectionRange.range, origin: sel.selectionOrigin.index};
  },

  colorToHtml(str: string): string {
    if (str.charAt(0) === "#" || str.substring(0,2) === "rgb") { // if color already correct format
      return str;
    } else {
      let result = CU.colorNameToHex(str);
      if (result === null || result === undefined) {
        throw new Error('Tried to convert an incorrect color name');
      }

      return result;
    }
  },

  asHAlignToHtml(align: string): string {
    switch (align) {
      case 'LeftAlign':
        return 'left';
      case 'HCenterAlign':
        return 'center';
      case 'RightAlign':
        return 'right';
      default:
        throw "Invalid HAlign passed in";
    }
  },

  colorNameToHex(color: string): ?string {
    if (typeof Constants.Colors[color.toLowerCase()] != 'undefined')
        return Constants.Colors[color.toLowerCase()];
    return undefined;
  },

  // indexStringToPair("(a,b)") := {row:a, col:b}
  indexStringToPair(indexString: string): PairObject<number> {
    var ab = indexString.substr(1, indexString.length-2).split(",");
    return {fst : parseInt(ab[0], 10), snd: parseInt(ab[1], 10)};
  },

  /* Gets the top left cell from the listKey. */
  // listKeyToListHead("I/reafe/(a,b)?(c,d)?LIST") := (a,b)"
  listKeyToListHead(listKey: string): (PairObject<number>) {
    if (listKey.split("?").length < 3 || listKey.split("?")[2] != "LIST") {
      throw new Error('There was an error with the format of listkey, no list head.');
    }
    return CU.indexStringToPair((listKey.split("?")[0]).split("/")[2]);
  },

  /* Gets the dimensions of the list from the listKey." */
  // listKeyToListDimensions("I/reafe/(a,b)?(c,d)?LIST") := (c,d)"
  listKeyToListDimensions(listKey: string): (PairObject<number>) {
    if (listKey.split("?").length < 3 || listKey.split("?")[2] != "LIST") {
      throw new Error('There was an error with the format of listkey, no dimensions.');
    }
    return CU.indexStringToPair(listKey.split("?")[1]);
  }
};

export default CU;
