/* @flow */

import {stream} from 'nu-stream';
import {parse, text} from 'bennu';
import _ from 'lodash';

import SheetStateStore from '../stores/ASSheetStateStore';

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

function intToCol(i: number): string {
  i--;
  const quo = Math.floor((i) / 26);
  const rem = (i) % 26;
  let code = '';
  if (quo > 0) {
      code += String.fromCharCode('A'.charCodeAt(0) + quo - 1);
  }
  code += String.fromCharCode('A'.charCodeAt(0) + rem);
  return code;
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

function excelIndexToString(exc: NakedExcelIndex): string {
  const {row, col, rowFixed, colFixed} = exc;
  return [
    dollarize(colFixed),
    intToCol(col),
    dollarize(rowFixed),
    row.toString()
  ].join('');
}

function stringToNakedExcelIndex(str: string): NakedExcelIndex {
  const DEFAULT = 'default';
  const maybeDollar = parse.optional(DEFAULT, text.character('$'));
  const letters: Parser<string> = // annotations for clarity
    parse.bind(parse.many1(text.letter),
      (pl) => parse.always(stream.toArray(pl).join(''))
    );
  const number: Parser<number> =
    parse.bind(parse.many1(text.digit),
      (digits) => parse.always(parseInt(stream.toArray(digits).join('')))
    );
  const excelParser: Parser<NakedExcelIndex> =
    parse.bind(maybeDollar,
      (colDollar) => parse.bind(letters,
        (parsedLetters) => {
          const col = colToInt(parsedLetters);
          return parse.bind(maybeDollar,
            (rowDollar) => parse.bind(number,
              (row) => {
                return parse.always({
                  row: row,
                  rowFixed: rowDollar === '$',
                  col: col,
                  colFixed: colDollar === '$'
                });
              }
            )
          )
        }
      )
    );
  return parse.run(excelParser, str);
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
        const refParts = parts[0].split(':').map(stringToNakedExcelIndex);
        if (refParts.length === 1) {
          _nakedRef = {
            tag: 'index',
            contents: refParts[0]
          };
        } else if (refParts.length === 2) {
          _nakedRef = {
            tag: 'range',
            first: refParts[0],
            second: refParts[1]
          };
        } else {
          throw new Error('Malformed Excel reference');
        }

        return new ASExcelRef(_nakedRef, _sheetId, _workbookId);

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

  toString(): string {
    const result = (() => {
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
    })();

    // TODO: figure out a better default for behavior of toString()
    // in presence of sheet names.
    // right now, we don't want to default-stringify A1:A4 into SHEET_ID!A1:A4
    // unless it's another sheet

    const globalSheetId = SheetStateStore.getCurrentSheetId();
    if (globalSheetId === this._sheetId) {
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
}
