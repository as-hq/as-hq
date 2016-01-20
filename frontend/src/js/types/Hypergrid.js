/* @flow */

import type ASIndex from '../classes/ASIndex';

import type {
  Style
} from '../types/Render';

import React from 'react';

type FourTuple<T> = [T,T,T,T];
type Pair<T> = [T,T];

export type CellBorder = FourTuple<?Pair<Pair<number>>>;

export type ASOverlaySpec = {
  id: string;
  renderElem: (style: Style) => ReactElement;
  initWidth: number;
  initHeight: number;
  offsetX: number;
  offsetY: number;
  left: number;
  top: number;
  loc: ?ASIndex;
};

export type GridSpec = {
  fixedColCount: number;
  fixedRowCount: number;
  scrollX: number;
  scrollY: number;
  firstVisibleColumn: number;
  firstVisibleRow: number;
  lastVisibleColumn: number;
  lastVisibleRow: number;
}
