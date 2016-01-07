/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASIndexObject
} from '../types/Eval';

import Constants from '../Constants';
import SheetStateStore from '../stores/ASSheetStateStore';

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
    sheetId = sheetId || SheetStateStore.getCurrentSheetId();
    return new ASIndex({
      tag: 'index',
      index: naked,
      sheetId: sheetId
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

  shift({dr, dc}: ({ dr: number; dc: number; })): ASIndex {
    return new ASIndex({ // safely shifts, preserving window bounds
      ...this.obj(),
      index: { row: (this.row + dr), col: (this.col + dc) }
    });
  }

  shiftByKey(e: SyntheticKeyboardEvent): ASIndex {
    return this.shift(Util.Key.keyShiftValue(e));
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

  above(): ASIndex { return this.shift({ dr: -1, dc: 0 }); }
  below(): ASIndex { return this.shift({ dr: 1, dc: 0 }); }
  toRight(): ASIndex { return this.shift({ dr: 0, dc: 1 }); }
  toLeft(): ASIndex { return this.shift({ dr: 0, dc: -1 }); }

  isInRange(rng: ASRange): boolean {
    const {row, col} = this;
    const {tl, br} = rng;

    return (
      col >= tl.col && col <= br.col &&
      row >= tl.row && row <= br.row
    );
  }
}
