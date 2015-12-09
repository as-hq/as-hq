/* @flow */

import type {
  Dict
} from './Base';

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ASRange,
  ASCell,
  ASLanguage
} from './Eval';

export type ASViewingWindow = {
  range: NakedRange;
};

export type ASSelection = {
  range: NakedRange;
  origin: NakedIndex;
};

export type ASFocusType = 'grid' | 'textbox' | 'editor';

export type ASClientWindow = {
  window: NakedRange;
  sheetId: string;
};

export type ASClientExpression = {
  expression: string;
  language: ASLanguage;
};

export type ASCellGrid =
  Dict<Array<Array<ASCell>>>;

export type ASCursorStyle = 'auto' | 'crosshair' | 'move';
