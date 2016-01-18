/* @flow */

import type ASCell from '../classes/ASCell';
import type ASRange from '../classes/ASRange';
import type ObjectDict from '../classes/ObjectDict';

import type {
  Dict
} from './Base';

import type {
  ASLanguage
} from './Eval';

import type {
  BarIndex,
  Bar
} from './Bar';

export type ASViewingWindow = ASRange;

export type ASFocusType = 'grid' | 'textbox' | 'editor';

export type ASClientWindow = {
  window: ASRange;
  sheetId: string;
};

// It is actually absurd that this type exists. cellDependencies should be removed from ASExpression.
export type ASClientExpression = {
  expression: string;
  language: ASLanguage;
};

export type ASCellGrid = Dict<Array<Array<ASCell>>>;

export type ASBarLines = ObjectDict<BarIndex, Bar>; // please help me think of a better name

export type ASCursorStyle = 'auto' | 'crosshair' | 'move';
