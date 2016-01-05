/* @flow */

import type {
  ASIndexObject
} from '../types/Eval';

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
  loc: ?ASIndexObject;
};
