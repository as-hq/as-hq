/* @flow */

import type {
  ASSelectionObject
} from '../types/Eval';

import type {
  PayloadSelection
} from '../types/Messages';

import ASIndex from './ASIndex';
import ASRange from './ASRange';

export default class ASSelection {
  _origin: ASIndex;
  _range: ASRange;
  _sheetId: string;

  constructor(obj: ASSelectionObject, sheetId?: ?string) {
    this._origin = ASIndex.fromNaked(obj.origin);
    this._range = ASRange.fromNaked(obj.range);

    if (sheetId) {
      this._sheetId = sheetId;
    }
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
