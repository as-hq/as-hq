/* @flow */

import type {
  ASAction
} from '../types/Actions';

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ASRange,
  ASLocation,
  ASSheet,
  ASWorkbook,
  ASCell
} from '../types/Eval';

import type {
  ASSelection,
  ASViewingWindow,
  ASClientExpression
} from '../types/State';

import type {
  PayloadSelection,
  ASBackendWorkbookSheet,
  ASClientWindow,
  ASMessageAction,
  ASClientMessage
} from '../types/Messages';

import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';
import T from '../AS/Types';

export default {
  /**************************************************************************************************************************/
  /* Type constructors */

  makeClientMessage(
    action: string,
    payloadTag: string,
    payloadContents: any
  ): ASClientMessage {
    return this.makeClientMessageRaw(action, { "tag": payloadTag,
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

  simpleToASRange(rng: NakedRange, sheetId?: string): ASRange {
    if (typeof(sheetId) == "undefined") sheetId = Store.getCurrentSheet().sheetId;
    return {tag: 'range', range: rng, sheetId: sheetId};
  },

  simpleToASIndex(idx: NakedIndex, sheetId?: string): ASIndex {
    if (typeof(sheetId) == "undefined") sheetId = Store.getCurrentSheet().sheetId;
    return {tag: 'index', index: idx, sheetId: sheetId};
  },

  indexToRange(ind: NakedIndex): NakedRange {
    return { tl: ind, br: ind };
  },

  indexToSelection(ind: NakedIndex): ASSelection {
    return { origin: ind, range: { tl: ind, br: ind } };
  },

  rangeToASWindow(rng: NakedRange): ASClientWindow {
    let sheetId = Store.getCurrentSheet().sheetId;
    return { window: rng, sheetId: sheetId };
  },

  rangeToASIndices(rng: NakedRange): Array<ASIndex> {
    let inds = [];
    for (var r = rng.tl.row; r <= rng.br.row; r++) {
      for (var c = rng.tl.col; c <= rng.br.col; c++) {
        inds.push(this.simpleToASIndex({row: r, col: c}));
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

  asLocationToSimple(loc: ASLocation): NakedRange {
    return (loc.tag === 'index') ? {tl: loc.index, br: loc.index} : loc.range;
  },

  asSelectionToSimple(sel: PayloadSelection): ASSelection {
    return {range: sel.selectionRange.range, origin: sel.selectionOrigin.index};
  }
}
