/* @flow */

import type {
  ASIndex
} from '../types/Eval';

import React from 'react';

type FourTuple<T> = [T,T,T,T];
type Pair<T> = [T,T];

export type CellBorder = FourTuple<?Pair<Pair<number>>>;

export type ASOverlaySpec = {
  id: string;
  renderElem: () => ReactElement;
  width: number;
  height: number;
  offsetX: number;
  offsetY: number;
  left: number;
  top: number;
  loc: ?ASIndex;
};
