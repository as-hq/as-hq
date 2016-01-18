/* @flow */

import type {
  ASLocation,
  ASCellProp,
  ASExpression,
  ASRangeObject
} from './Eval';

export type CondFormatRule = {
  tag: 'CondFormatRule';
  condFormatRuleId: string;
  cellLocs: Array<ASRangeObject>;
  formatMapConstructor: FormatMapConstructor;
};

export type FormatMapConstructor = BoolFormatMapConstructor | LambdaFormatMapConstructor;

export type BoolFormatMapConstructor = {
  tag: "BoolFormatMapConstructor";
  boolFormatMapCondition: BoolCondition;
  boolFormatMapProps: [ASCellProp];
};

export type LambdaFormatMapConstructor = {
  tag: "LambdaFormatMapConstructor";
  contents: string;
};

export type BoolCondition =
  CustomCondition
  | GreaterThanCondition
  | LessThanCondition
  | GeqCondition
  | LeqCondition
  | EqualsCondition
  | NotEqualsCondition
  | IsEmptyCondition
  | IsNotEmptyCondition
  | IsBetweenCondition
  | IsNotBetweenCondition;

export type CustomCondition = {
  tag: 'CustomCondition';
  contents: ASExpression;
};

export type GreaterThanCondition = {
  tag: 'GreaterThanCondition';
  contents: ASExpression;
};

export type LessThanCondition = {
  tag: 'LessThanCondition';
  contents: ASExpression;
};

export type GeqCondition = {
  tag: 'GeqCondition';
  contents: ASExpression;
};

export type LeqCondition = {
  tag: 'LeqCondition';
  contents: ASExpression;
};

export type EqualsCondition = {
  tag: 'EqualsCondition';
  contents: ASExpression;
};

export type NotEqualsCondition = {
  tag: 'NotEqualsCondition';
  contents: ASExpression;
};

export type IsEmptyCondition = {
  tag: 'IsEmptyCondition';
};

export type IsNotEmptyCondition = {
  tag: 'IsNotEmptyCondition';
};

export type IsBetweenCondition = {
  tag: 'IsBetweenCondition';
  contents: [ASExpression, ASExpression];
};

export type IsNotBetweenCondition = {
  tag: 'IsNotBetweenCondition';
  contents: [ASExpression, ASExpression];
};
