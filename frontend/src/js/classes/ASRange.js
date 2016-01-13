/* @flow */

import type {
  NakedRange,
  ASRangeObject
} from '../types/Eval';

import type {
  ASClientWindow
} from '../types/Messages';

import Util from '../AS/Util';
import Constants from '../Constants';
import SheetStateStore from '../stores/ASSheetStateStore';

import _ from 'lodash';

import ASExcelRef from './ASExcelRef';
import ASIndex from './ASIndex';
import ASSelection from './ASSelection';

function validateCorners(tl: ASIndex, br: ASIndex) {
  return tl.isAboveAndLeft(br);
}

function orientRange(rng: NakedRange): NakedRange {
  const tl = {row: Math.min(rng.tl.row, rng.br.row), col: Math.min(rng.tl.col, rng.br.col)},
        br = {row: Math.max(rng.tl.row, rng.br.row), col: Math.max(rng.tl.col, rng.br.col)};
  return {tl: tl, br: br};
};

export default class ASRange {
  _tl: ASIndex;
  _br: ASIndex;
  _sheetId: string;

  get tl(): ASIndex { return this._tl; }
  get br(): ASIndex { return this._br; }
  get sheetId(): string { return this._sheetId; }

  constructor(obj: ASRangeObject) {
    const {range: unorientedRange, sheetId} = obj;
    const {tl: ttl, br: tbr} = orientRange(unorientedRange);

    const tl = ASIndex.fromNaked(ttl), br = ASIndex.fromNaked(tbr);

    this._tl = tl;
    this._br = br;
    this._sheetId = sheetId;
  }

  static fromASIndices({ tl, br }: ({ tl: ASIndex, br: ASIndex })): ASRange {
    if (tl.sheetId !== br.sheetId) {
      throw new Error('Sheet IDs are not the same for tl and br');
    }

    const {sheetId} = tl;

    return ASRange.fromNaked({
      tl: tl.obj().index,
      br: br.obj().index
    }, sheetId);
  }

  static fromExcelString(excStr: string): ASRange {
    return ASExcelRef.fromString(excStr).toRange();
  }

  static fromNaked(naked: NakedRange, sheetId?: ?string): ASRange {
    sheetId = sheetId || SheetStateStore.getCurrentSheetId();
    return new ASRange({
      tag: 'range',
      range: naked,
      sheetId: sheetId
    });
  }

  toNaked(): NakedRange {
    return this.obj().range;
  }

  obj(): ASRangeObject {
    return ({
      tag: 'range',
      range: { tl: this.tl.toNaked(), br: this.br.toNaked() },
      sheetId: this.sheetId
    });
  }

  toIndices2d(): Array<Array<ASIndex>> {
    return _.range(this.tl.row, this.br.row + 1).map((row) =>
      _.range(this.tl.col, this.br.col + 1).map((col) =>
        ASIndex.fromNaked({ row: row, col: col }, this.sheetId)
      )
    );
  }

  toIndices(): Array<ASIndex> {
    return [].concat.apply([], this.toIndices2d());
  }

  toExcel(): ASExcelRef {
    return ASExcelRef.fromRange(this);
  }

  toClientWindow(sheetId?: string): ASClientWindow {
    sheetId = sheetId || SheetStateStore.getCurrentSheetId();
    return ({
      window: this.toNaked(),
      sheetId: this.sheetId
    });
  }

  toSelection(): ASSelection {
    return new ASSelection({
      origin: this.tl.obj().index,
      range: this.obj().range
    });
  }

  isIndex(): boolean {
    return this.tl.equals(this.br);
  }

  getBL(): ASIndex {
    return ASIndex.fromNaked({
      row: this.br.row,
      col: this.tl.col
    }, this.sheetId);
  }

  getTR(): ASIndex {
    return ASIndex.fromNaked({
      row: this.tl.row,
      col: this.br.col
    }, this.sheetId);
  }

  getTopRow(): ASRange {
    const {tl, br} = this;
    return ASRange.fromASIndices({
      tl: tl,
      br: ASIndex.fromNaked({
        row: tl.row, col: br.col
      })
    });
  }

  getLeftColumn(): ASRange {
    const {tl, br} = this;
    return ASRange.fromASIndices({
      tl: tl,
      br: ASIndex.fromNaked({
        row: br.row, col: tl.col
      })
    });
  }

  extendByCache(): ASRange {
    const {scrollCacheX, scrollCacheY} = Constants;
    return ASRange.fromNaked({
      tl: this.tl.shift({ dr: -scrollCacheY, dc: -scrollCacheX }),
      br: this.br.shift({ dr: scrollCacheY, dc: scrollCacheX })
    }, this.sheetId);
  }

  shift(delta: ({ dr: number; dc: number; })): ASRange {
    return ASRange.fromNaked({
      tl: this.tl.shift(delta),
      br: this.br.shift(delta)
    }, this.sheetId);
  }

  shiftByKey(e: SyntheticKeyboardEvent): ASRange {
    return this.shift(Util.Key.keyShiftValue(e));
  }

  contains(idx: ASIndex): boolean {
    return idx.isInRange(this);
  }

  intersectsWith(other: ASRange): boolean {
    const {tl, br} = this;
    return (
      tl.col <= other.br.col
      && br.col >= other.tl.col
      && tl.row <= other.br.row
      && br.row >= other.tl.row
    );
  }
}
