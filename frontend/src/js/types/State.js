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

export type FocusedElement = 'grid' | 'textbox' | 'editor' | 'header' | 'bottompane';

export type BottomPaneType = 'header_output' | 'errors' | 'cell_output' | 'no_pane';

export type ASClientWindow = {
  window: ASRange;
  sheetId: string;
};

export type ASCellGrid = Dict<Array<Array<ASCell>>>;

export type ASBarLines = ObjectDict<BarIndex, Bar>; // please help me think of a better name

export type ASCursorStyle = 'auto' | 'crosshair' | 'move';
