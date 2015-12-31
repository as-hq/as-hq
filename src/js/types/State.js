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
  ASLanguage,
} from './Eval';

import type { 
  BarIndex, 
  Bar
} from './Bar';

export type ASViewingWindow = {
  range: NakedRange;
};

export type ASFocusType = 'grid' | 'textbox' | 'editor';

export type ASClientWindow = {
  window: NakedRange;
  sheetId: string;
};

// It is actually absurd that this type exists. cellDependencies should be removed from ASExpression. 
export type ASClientExpression = {
  expression: string;
  language: ASLanguage;
};

export type ASCellGrid = Dict<Array<Array<ASCell>>>;

export type ASBarLines = { [key: BarIndex]: Bar }; // please help me think of a better name

export type ASCursorStyle = 'auto' | 'crosshair' | 'move';
