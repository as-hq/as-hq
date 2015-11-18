/* @flow */

import type {
  Dict
} from './Base';

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ASRange,
  ASCell
} from './Eval';

export type ASViewingWindow = {
  range: NakedRange;
};

export type ASSelection = {
  range: NakedRange;
  origin: NakedIndex;
};

export type ASFocusType = 'grid' | 'textbox' | 'editor';

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

export type ASClientWindow = {
  window: NakedRange;
  sheetId: string;
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

export type ASCellStore =
  Dict<Array<Array<ASCell>>>;

export type ASCursorStyle = 'auto' | 'crosshair' | 'move';
