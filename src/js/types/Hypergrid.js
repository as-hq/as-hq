/* @flow */

import type {
  ASIndex
} from '../types/Eval';

type FourTuple<T> = [T,T,T,T];
type Pair<T> = [T,T];

export type CellBorder = FourTuple<?Pair<Pair<number>>>;

export type ASOverlaySpec = {
  id: string;
  src: string;
  width: number;
  height: number;
  offsetX: number;
  offsetY: number;
  left: number;
  top: number;
  loc: ASIndex;
};

