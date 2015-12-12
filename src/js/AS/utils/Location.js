/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASSelection
} from '../../types/Eval';

import type {
  ASOverlaySpec
} from '../../types/Hypergrid';

import Constants from '../../Constants';

let LocationUtils = {
  getSafeRow(r: number): number {
    return Math.min(Math.max(r, 1), Constants.numRows);
  },

  getSafeCol(c: number): number {
    return Math.min(Math.max(c, 1), Constants.numCols);
  },

  getSafeRange(rng: NakedRange): NakedRange {
    return {tl: LocationUtils.getSafeIndex(rng.tl),
            br: LocationUtils.getSafeIndex(rng.br)};
  },

  _isContainedInLoc(col: number, row: number, loc: NakedRange): boolean {
    let {tl, br} = loc;
    return (col >= tl.col && col <= br.col &&
            row >= tl.row && row <= br.row);
  },

  isContainedInLocs(col: number, row: number, locs: Array<NakedRange>): boolean {
    return locs.some((loc) => LocationUtils._isContainedInLoc(col, row, loc));
  },

  getSafeSelection(sel: ASSelection): ASSelection {
    return { origin: LocationUtils.getSafeIndex(sel.origin), range: LocationUtils.getSafeRange(sel.range) };
  },

  originIsCornerOfSelection(sel: ASSelection): boolean {
    let {origin, range: {tl, br}} = sel;
    return (origin.row === tl.row || origin.row == br.row) &&
           (origin.col == tl.col || origin.col == br.col);
  },

  getSafeIndex(idx: NakedIndex): NakedIndex {
    return {col: LocationUtils.getSafeCol(idx.col), row: LocationUtils.getSafeRow(idx.row)};
  },

  extendRangeByCache(rng: NakedRange): NakedRange {
    let tl = LocationUtils.getSafeIndex({row: rng.tl.row-Constants.scrollCacheY, col: rng.tl.col-Constants.scrollCacheX}),
        br = LocationUtils.getSafeIndex({row: rng.br.row+Constants.scrollCacheY, col: rng.br.col+Constants.scrollCacheX});
    return { tl: tl, br: br };
  },

  offsetRange(rng: NakedRange, dY: number, dX: number): NakedRange {
    let {tl, br} = rng;
    return {tl: {col: tl.col + dX, row: tl.row + dY},
            br: {col: br.col + dX, row: br.row + dY}};
  },

    // Check if the mouse location is in the square box for draggging
  mouseLocIsContainedInBox(
    mouseLocX: number,
    mouseLocY: number,
    topLeftBoxObj: ?HGPoint,
    boxWidth: number
  ): boolean {
    if (topLeftBoxObj == null) {
      return false;
    }
    let xInBounds = mouseLocX >= topLeftBoxObj.x &&
                    mouseLocX <= topLeftBoxObj.x + boxWidth,
        yInBounds = mouseLocY >= topLeftBoxObj.y &&
                    mouseLocY <= topLeftBoxObj.y + boxWidth;
    return xInBounds && yInBounds;
  },

  isIndex(simpleLocation: NakedRange): boolean {
    return (simpleLocation.tl.row === simpleLocation.br.row) &&
           (simpleLocation.tl.col === simpleLocation.br.col);
  },

  orientRange(rng: NakedRange): NakedRange {
    var tl = {row: Math.min(rng.tl.row, rng.br.row), col: Math.min(rng.tl.col, rng.br.col)},
        br = {row: Math.max(rng.tl.row, rng.br.row), col: Math.max(rng.tl.col, rng.br.col)};
    return {tl: tl, br: br};
  },

  shiftIndex(ind: NakedIndex, dr: number, dc: number): NakedIndex {
    return {row: ind.row + dr, col: ind.col + dc};
  }
};

export default LocationUtils;
