/* @flow */

import type {
  ASSelectionObject
} from '../types/Eval';

import type {
  PayloadSelection
} from '../types/Messages';

import SheetStateStore from '../stores/ASSheetStateStore';

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
    this._origin = ASIndex.fromNaked(obj.origin);
    this._range = ASRange.fromNaked(obj.range);
    this._sheetId = sheetId || SheetStateStore.getCurrentSheetId();
  }

  static defaultSelection(): ASSelection {
    return ASIndex.fromNaked({ row: 1, col: 1}).toSelection();
  }

  static fromPayload(payload: PayloadSelection): ASSelection {
    const {
      selectionRange: {range, sheetId},
      selectionOrigin: {index: origin}
    } = payload;

    return new ASSelection({
      range: range,
      origin: origin
    }, sheetId);
  }

  originIsCorner(): boolean {
    const rng = this._range;
    return [rng.tl, rng.br, rng.getBL(), rng.getTR()].
      some((x) => x.equals(this._origin));
  }
}
