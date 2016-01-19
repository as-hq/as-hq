/* @flow */

import type {
  ASCellProp
} from '../types/Eval';

import type {
  CondFormatRule,
  BoolCondition,
  FormatMapConstructor
} from '../types/CondFormat';

import ASRange from './ASRange';

import shortid from 'shortid';

export default class ASCondFormatRule {
  _condFormatRuleId: string;
  _formatMapConstructor: FormatMapConstructor;
  _cellLocs: Array<ASRange>;

  get condFormatRuleId(): string { return this._condFormatRuleId; }
  get formatMapConstructor(): FormatMapConstructor { return this._formatMapConstructor; }
  get cellLocs(): Array<ASRange> { return this._cellLocs; }

  static fromClasses({ condFormatRuleId, formatMapConstructor, cellLocs }: ({
    condFormatRuleId: ?string;
    formatMapConstructor: FormatMapConstructor;
    cellLocs: Array<ASRange>;
  })): ASCondFormatRule {
    return new ASCondFormatRule({
      tag: 'CondFormatRule',
      condFormatRuleId: condFormatRuleId || ASCondFormatRule.getNewRuleId(),
      formatMapConstructor: formatMapConstructor,
      cellLocs: cellLocs.map((r) => r.obj())
    });
  }

  static getNewRuleId(): string {
    return shortid.generate();
  }

  constructor(obj: CondFormatRule) {
    this._condFormatRuleId = obj.condFormatRuleId;
    this._formatMapConstructor = obj.formatMapConstructor;
    this._cellLocs = obj.cellLocs.map((r) => new ASRange(r));
  }

  obj(): CondFormatRule {
    const {condFormatRuleId, formatMapConstructor, cellLocs} = this;

    return ({
      tag: 'CondFormatRule',
      condFormatRuleId: condFormatRuleId,
      formatMapConstructor: formatMapConstructor,
      cellLocs: cellLocs.map((r) => r.obj())
    });
  }

  appliesTo(rng: ASRange): boolean {
    return this.cellLocs.some((r) => rng.intersectsWith(r));
  }
}
