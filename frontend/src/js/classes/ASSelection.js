/* @flow */

import type {
  ASSelectionObject,
  NakedIndex
} from '../types/Eval';

import type { Offset } from '../types/Eval';

import WorkbookStore from '../stores/ASWorkbookStore';

import ASIndex from './ASIndex';
import ASRange from './ASRange';

export default class ASSelection {
  _origin: ASIndex;
  _range: ASRange;
  _sheetId: string;

  get origin(): ASIndex { return this._origin; }
  get range(): ASRange { return this._range; }
  get sheetId(): string { return this._sheetId; }

  constructor(obj: ASSelectionObject, sheetId?: ?string) {
    this._sheetId = sheetId || WorkbookStore.getCurrentSheetId();
    this._origin = ASIndex.fromNaked(obj.origin, this._sheetId);
    this._range = ASRange.fromNaked(obj.range, this._sheetId);
  }

  equals(other: ASSelection): boolean {
    return (
      this.origin.equals(other.origin) &&
      this.range.equals(other.range) &&
      this.sheetId === other.sheetId
    );
  }

  static defaultSelection(sheetId?: string): ASSelection {
    return ASIndex.fromNaked({ row: 1, col: 1}, sheetId).toSelection();
  }

  static fromASLocations({ origin, range, sheetId }: ({
    origin: ASIndex;
    range: ASRange;
    sheetId?: ?string;
  })): ASSelection {
    const updSheetId = sheetId || WorkbookStore.getCurrentSheetId();
    return new ASSelection({
      origin: origin.obj().index,
      range: range.obj().range
    }, updSheetId);
  }

  static fromExcelStrings({ origin, range, sheetId }: ({
    origin: string;
    range: string;
    sheetId?: string;
  })): ASSelection {

    return ASSelection.fromASLocations({
      origin: ASIndex.fromExcelString(origin),
      range: ASRange.fromExcelString(range),
      sheetId: sheetId
    });
  }

  toExcelString(): string {
    return this.range.toExcel().toString();
  }

  // non-mutating
  changeSheet(sheetId: string): ASSelection {
    const {origin, range} = this;
    return new ASSelection({
      origin: origin.obj().index,
      range: range.obj().range
    }, sheetId);
  }

  originIsCorner(): boolean {
    const rng = this._range;
    return [rng.tl, rng.br, rng.getBL(), rng.getTR()].
      some((x) => x.equals(this._origin));
  }

  /**
   * The drag origin is the corner opposite the actual origin.
   * It is the selection 'lead'.
   * @return {NakedIndex}
   */
  getDragOrigin(): NakedIndex {
    const {tl, br} = this._range;
    const cols = [tl.col, br.col];
    const rows = [tl.row, br.row];
    const {col: oCol, row: oRow} = this._origin;

    return {
      col: cols[
        (cols.indexOf(oCol) + 1) % 2 // retrieve the non-origin column
      ],
      row: rows[
        (rows.indexOf(oRow) + 1) % 2 // retrieve the non-origin row
      ]
    };
  }

  /**
   * [shifts the selection, does not mutate.]
   * @param  {Offset} offset
   * @param  {boolean} extend [whether to extend the selection (e.g. the shift key is pressed) or to move it]
   */
  shift({dX, dY}: Offset, extend: boolean = false): ASSelection {
    const {col: anchorX, row: anchorY} = this._origin;

    if (extend) {
      const {col: leadX, row: leadY} = this.getDragOrigin();
      const leadX_ = leadX + dX;
      const leadY_ = leadY + dY;
      const tl = {
        col: Math.min(leadX_, anchorX),
        row: Math.min(leadY_, anchorY)
      };
      const br = {
        col: Math.max(leadX_, anchorX),
        row: Math.max(leadY_, anchorY)
      };

      return new ASSelection({
        origin: this._origin,
        range: {tl, br}
      });

    } else {
      return ASIndex.fromNaked({
        col: anchorX + dX,
        row: anchorY + dY
      }).toSelection();
    }
  }

}
