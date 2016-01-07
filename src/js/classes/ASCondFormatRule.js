/* @flow */

import type {
  ASCellProp
} from '../types/Eval';

import type {
  CondFormatRule,
  CondFormatCondition
} from '../types/CondFormat';

import ASRange from './ASRange';

export default class ASCondFormatRule {
  _condFormatRuleId: string;
  _condFormat: ASCellProp;
  _condition: CondFormatCondition;
  _cellLocs: Array<ASRange>;

  get condFormatRuleId(): string { return this._condFormatRuleId; }
  get condFormat(): ASCellProp { return this._condFormat; }
  get condition(): CondFormatCondition { return this._condition; }
  get cellLocs(): Array<ASRange> { return this.cellLocs; }

  static fromClasses({ condFormatRuleId, condFormat, condition, cellLocs }: ({
    condFormatRuleId: string;
    condFormat: ASCellProp;
    condition: CondFormatCondition;
    cellLocs: Array<ASRange>;
  })): ASCondFormatRule {
    return new ASCondFormatRule({
      tag: 'CondFormatRule',
      condFormatRuleId: condFormatRuleId,
      condFormat: condFormat,
      condition: condition,
      cellLocs: cellLocs.map((r) => r.obj())
    });
  }

  constructor(obj: CondFormatRule) {
    this._condFormatRuleId = obj.condFormatRuleId;
    this._condFormat = obj.condFormat;
    this._condition = obj.condition;
    this._cellLocs = obj.cellLocs.map((r) => new ASRange(r));
  }

  obj(): CondFormatRule {
    const {condFormatRuleId, condFormat, condition, cellLocs} = this;

    return ({
      tag: 'CondFormatRule',
      condFormatRuleId: condFormatRuleId,
      condFormat: condFormat,
      condition: condition,
      cellLocs: cellLocs.map((r) => r.obj())
    });
  }
}
