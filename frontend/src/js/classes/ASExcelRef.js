/* @flow */

import {stream} from 'nu-stream';
import {parse, text} from 'bennu';
import invariant from 'invariant';
import _ from 'lodash';

import WorkbookStore from '../stores/ASWorkbookStore';

import ASIndex from './ASIndex';
import ASRange from './ASRange';

type NakedExcelIndex = {
  row: number;
  col: number;
  rowFixed: boolean;
  colFixed: boolean;
};

type NakedExcelRef = {
  tag: 'index';
  contents: NakedExcelIndex;
} | {
  tag: 'range';
  first: NakedExcelIndex;
  second: NakedExcelIndex;
};

// Converts an integer (0 - 25) to the corresponding letter
function intToLetter(i : number): string {
  return String.fromCharCode('A'.charCodeAt(0) + i);
}

function intToCol(i: number): string {
  if (i <= 26) { return intToLetter(i - 1);
  } else {
    const quo = Math.floor((i - 1) / 26);
    const rem = (i - 1) % 26;
    return intToCol(quo) + intToLetter(rem);
  }
}

function letterToInt(letter: string): number {
  return letter.charCodeAt(0) - 'A'.charCodeAt(0) + 1;
}

function colToInt(col: string): number {
  return col.toUpperCase().split('').reduce((acc, cur) => {
    return acc * 26 + letterToInt(cur);
  }, 0);
}

function dollarize(b: boolean): string {
  return b ? '$' : '';
}

function rowParamsToRowString({row, rowFixed}: ({
  row: number;
  rowFixed: boolean;
})): string {
  if (row === Infinity) {
    return '';
  } else {
    return [dollarize(rowFixed), row.toString()].join('');
  }
}

function excelIndexToString(exc: NakedExcelIndex): string {
  const {row, col, rowFixed, colFixed} = exc;
  return [
    dollarize(colFixed),
    intToCol(col),
    rowParamsToRowString({ row: row, rowFixed: rowFixed })
  ].join('');
}

function toggleNakedIndex(myIdx: NakedExcelIndex): NakedExcelIndex {
  const idx = myIdx;
  if (idx.rowFixed && idx.colFixed) {
    idx.rowFixed = true;
    idx.colFixed = false;
  } else if (idx.colFixed && !idx.rowFixed) {
    idx.rowFixed = false;
    idx.colFixed = false;
  } else {
    idx.rowFixed = ! idx.rowFixed;
    idx.colFixed = ! idx.colFixed;
  }
  return idx;
}

const DEFAULT = 'default';
const pMaybeDollar = parse.optional(DEFAULT, text.character('$'));
const pLetters: Parser<string> = // annotations for clarity
  parse.bind(parse.many1(text.letter),
    (pl) => parse.always(stream.toArray(pl).join(''))
  );
const pNumber: Parser<number> =
  parse.bind(parse.many1(text.digit),
    (digits) => parse.always(parseInt(stream.toArray(digits).join('')))
  );
const pEmptyString: Parser<string> = text.string('');
const pColon: Parser<string> = text.character(':');

const pColumn: Parser<{
  col: number;
  colFixed: boolean;
}> =
  parse.bind(pMaybeDollar,
    (colDollar) => parse.bind(pLetters,
      (parsedLetters) => {
        const col = colToInt(parsedLetters);
        return parse.always({
          col: col,
          colFixed: colDollar === '$'
        });
      }
    )
  );

const pRow: Parser<{
  row: number;
  rowFixed: boolean;
}> =
  parse.bind(pMaybeDollar,
    (rowDollar) => parse.bind(pNumber,
      (row) => {
        return parse.always({
          row: row,
          rowFixed: rowDollar === '$'
        });
      }
    )
  );

const nakedExcelIndex: Parser<NakedExcelIndex> =
  parse.bind(pColumn, (colObj) =>
    parse.bind(pRow, (rowObj) =>
      parse.always({
        ...colObj, ...rowObj
      })
    )
  );

const a2aRef: Parser<NakedExcelRef> =
  parse.bind(pColumn, (start) =>
    parse.next(pColon, parse.bind(pColumn, (end) =>
      parse.always({
        tag: 'range',
        first: {
          row: 1, rowFixed: true,
          ...start
        },
        second: {
          row: Infinity, rowFixed: true,
          ...end
        }
      })
    ))
  );

const a12aRef: Parser<NakedExcelRef> =
  parse.bind(nakedExcelIndex, (idx1) =>
    parse.next(pColon, parse.bind(pColumn, (endCol) =>
      parse.always({
        tag: 'range',
        first: idx1,
        second: {
          row: Infinity, rowFixed: true,
          ...endCol
        }
      })
    ))
  );

const rangeRef: Parser<NakedExcelRef> =
  parse.bind(nakedExcelIndex, (idx1) =>
    parse.next(pColon, parse.bind(nakedExcelIndex, (idx2) =>
      parse.always({
        tag: 'range',
        first: idx1,
        second: idx2
      })
    ))
  );

const indexRef: Parser<NakedExcelRef> =
  parse.bind(nakedExcelIndex, (idx) =>
    parse.always({
      tag: 'index',
      contents: idx
    })
  );

const nakedExcelRef: Parser<NakedExcelRef> =
  parse.either(
    parse.attempt(rangeRef),
    parse.either(
      parse.attempt(a2aRef),
      parse.either(
        parse.attempt(a12aRef),
        indexRef
      )
    )
  );

function parseExcelRef(ref: string): NakedExcelRef {
  return parse.run(nakedExcelRef, ref);
}

export default class ASExcelRef {
  _nakedRef: NakedExcelRef;
  _sheetId: ?string;
  _workbookId: ?string;

  constructor(_nakedRef: NakedExcelRef, _sheetId: ?string, _workbookId: ?string) {
    this._nakedRef = _nakedRef;
    this._sheetId = _sheetId;
    this._workbookId = _workbookId;
  }

  static fromString(str: string): ASExcelRef {
    const parts = str.split('!').reverse();
    // reverse so that ['A2:B2'] is subset of ['A2:B2', 'sheetid'] etc

    let _nakedRef, _sheetId, _workbookId;
    switch (parts.length) {
      case 3:
        _workbookId = parts[2];
      case 2:
        _sheetId = parts[1];
      case 1:
        const strRef = parts[0];
        _nakedRef = parseExcelRef(strRef);
        const ret = new ASExcelRef(_nakedRef, _sheetId, _workbookId);
        return ret;

      default:
        throw new Error('Can\'t be more than 3 parts');
    }
  }

  static fromIndex(idx: ASIndex): ASExcelRef {
    return new ASExcelRef({
      tag: 'index',
      contents: {
        row: idx.row,
        col: idx.col,
        rowFixed: false,
        colFixed: false
      }
    }, idx.sheetId);
  }

  static fromRange(rng: ASRange): ASExcelRef {
    let {tl, br} = rng;
    return new ASExcelRef({
      tag: 'range',
      first: { row: tl.row, col: tl.col, rowFixed: false, colFixed: false },
      second: { row: br.row, col: br.col, rowFixed: false, colFixed: false }
    }, rng.sheetId);
  }

  toLocalRefString(): string {
    if (this._nakedRef.tag === 'index') {
      return excelIndexToString(this._nakedRef.contents);
    } else {
      const {first, second} = this._nakedRef;

      if (_.isEqual(first, second)) {
        return excelIndexToString(first);
      } else {
        return [first, second].map(excelIndexToString).join(':');
      }
    }
  }

  toString(): string {
    const result = this.toLocalRefString();

    // TODO: figure out a better default for behavior of toString()
    // in presence of sheet names.
    // right now, we don't want to default-stringify A1:A4 into SHEET_ID!A1:A4
    // unless it's another sheet

    const globalSheetId = WorkbookStore.getCurrentSheetId();
    if (globalSheetId === this._sheetId || this._sheetId === undefined) {
      return result;
    } else {
      return [this._sheetId, result].join('!');
    }

/*
    if (this._workbookId) {
      return [this._workbookId, this._sheetId, result].join('!');
    } else if (this._sheetId) {
      return [this._sheetId, result].join('!');
    } else {
      return result;
    } */
  }

  toIndex(): ASIndex {
    if (this._nakedRef.tag === 'index') {
      return ASIndex.fromNaked(this._nakedRef.contents, this._sheetId);
    } else {
      throw new Error('Ref is not an index');
    }
  }

  toRange(): ASRange {
    if (this._nakedRef.tag === 'range') {
      const {first, second} = this._nakedRef;
      return ASRange.fromNaked({
        tl: ASIndex.fromNaked(first),
        br: ASIndex.fromNaked(second)
      }, this._sheetId);
    } else {
      return this.toIndex().toRange();
    }
  }

  toggle(): ASExcelRef {
    switch(this._nakedRef.tag) {
      case 'index': {
        const contents = toggleNakedIndex(this._nakedRef.contents);
        const nref = {tag: 'index', contents};
        return new ASExcelRef(nref, this._sheetId, this._workbookId);
      }

      case 'range': {
        // $FlowFixMe ::ALEX::
        const first = toggleNakedIndex(this._nakedRef.first);
        // $FlowFixMe ::ALEX::
        const second = toggleNakedIndex(this._nakedRef.second);
        const nref = {tag: 'range', first, second};
        return new ASExcelRef(nref, this._sheetId, this._workbookId);
      }

      default:
        invariant('ASExcelRef tag was neither index nor range.');
        return new ASExcelRef(this._nakedRef, this._sheetId, this._workbookId);
    }
  }
}
