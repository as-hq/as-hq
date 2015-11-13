/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ASRange
} from './Eval';

export type ASViewingWindow = ASRange;

export type ASSelection = {
  range: NakedRange;
  origin: NakedIndex;
};

export type PythonClientLang = {
  Display: 'Python';
  Server: 'Python';
  Editor: 'python';
};

export type ExcelClientLang = {
  Display: 'Excel';
  Server: 'Excel';
  Editor: 'python';
};

export type RClientLang = {
  Display: 'R';
  Server: 'R';
  Editor: 'r';
};

export type SQLClientLang = {
  Display: 'SQL';
  Server: 'SQL';
  Editor: 'mysql';
};

export type ASClientLanguage =
  PythonClientLang
  | RClientLang
  | ExcelClientLang
  | SQLClientLang;

export type ASClientExpression = {
  expression: string;
  language: ASClientLanguage;
};
