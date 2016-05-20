/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASIndexObject,
  Offset
} from '../types/Eval';

import Constants from '../Constants';
import WorkbookStore from '../stores/ASWorkbookStore';

import Util from '../AS/Util';

import ASExcelRef from './ASExcelRef';
import ASRange from './ASRange';
import ASSelection from './ASSelection';

function toSafeRow(r: number): number {
  return Math.min(Math.max(r, 1), Constants.numRows);
}

function toSafeCol(c: number): number {
  return Math.min(Math.max(c, 1), Constants.numCols);
}

function toSafeIndex({row, col}: NakedIndex): NakedIndex {
  return {
    col: toSafeCol(col), row: toSafeRow(row)
  };
}

function validateSheetId(sheetId: string): boolean {
  return true; // TODO
}

export default class ASIndex {
  _row: number;
  _col: number;
  _sheetId: string;

  get row(): number { return this._row; }
  get col(): number { return this._col; }
  get sheetId(): string { return this._sheetId; }

  constructor(obj: ASIndexObject) {
    const {index: unsafeIndex, sheetId} = obj;
    const {row, col} = toSafeIndex(unsafeIndex);

    if (!validateSheetId(sheetId)) {
      throw new Error('Invalid sheet ID');
    }

    this._row = row;
    this._col = col;
    this._sheetId = sheetId;
  }

  static makeIndices(indexObjects: Array<ASIndexObject>): Array<ASIndex> {
    return indexObjects.map((obj) => new ASIndex(obj));
  }

  static fromExcelString(excStr: string): ASIndex {
    return ASExcelRef.fromString(excStr).toIndex();
  }

  static fromGridCell(gridCell: HGPoint): ASIndex {
    return ASIndex.fromNaked({ row: gridCell.y, col: gridCell.x });
  }

  static fromNaked(naked: NakedIndex, sheetId?: ?string): ASIndex {
    const sid = sheetId || WorkbookStore.getCurrentSheetId();
    return new ASIndex({
      tag: 'index',
      index: naked,
      sheetId: sid
    });
  }

  toNaked(): NakedIndex {
    return this.obj().index;
  }

  obj(): ASIndexObject {
    return ({
      tag: 'index',
      index: { row: this.row, col: this.col },
      sheetId: this.sheetId
    });
  }

  toExcel(): ASExcelRef {
    return ASExcelRef.fromIndex(this);
  }

  toRange(): ASRange {
    const nakedIndex = { row: this.row, col: this.col };
    return new ASRange({
      tag: 'range',
      range: {
        tl: nakedIndex, br: nakedIndex
      },
      sheetId: this.sheetId
    });
  }

  toSelection(): ASSelection {
    return this.toRange().toSelection();
  }

  changeSheet(sheetId: string): ASIndex {
    return ASIndex.fromNaked(
      this.toNaked(),
      sheetId
    );
  }

  shift({dY, dX}: Offset): ASIndex {
    return new ASIndex({ // safely shifts, preserving window bounds
      ...this.obj(),
      index: { row: (this.row + dY), col: (this.col + dX) }
    });
  }

  shiftByKey(e: SyntheticKeyboardEvent): ASIndex {
    return this.shift(Util.Key.keyToOffset(e));
  }

  isAboveAndLeft(idx: ASIndex): boolean {
    return (this.row <= idx.row && this.col <= idx.col);
  }

  equals(other: ASIndex): boolean {
    return (
      this.row === other.row &&
      this.col === other.col &&
      this.sheetId === other.sheetId
    );
  }

  above(): ASIndex { return this.shift({ dY: -1, dX: 0 }); }
  below(): ASIndex { return this.shift({ dY: 1, dX: 0 }); }
  toRight(): ASIndex { return this.shift({ dY: 0, dX: 1 }); }
  toLeft(): ASIndex { return this.shift({ dY: 0, dX: -1 }); }

  getRowRange(): ASRange {
    return new ASRange.fromNaked({
      tl: { row: this.row, col: 1 },
      br: { row: this.row, col: Infinity }
    });
  }

  getColumnRange(): ASRange {
    return new ASRange.fromNaked({
      tl: { row: 1, col: this.col },
      br: { row: Infinity, col: this.col }
    });
  }

  isInRange(rng: ASRange): boolean {
    const {row, col} = this;
    const {tl, br} = rng;

    return (
      col >= tl.col && col <= br.col &&
      row >= tl.row && row <= br.row
    );
  }
}
