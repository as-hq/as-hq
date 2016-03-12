/* @flow */

import React from 'react';

type FourTuple<T> = [T,T,T,T];
type Pair<T> = [T,T];

export type CellBorder = FourTuple<?Pair<Pair<number>>>;

export type GridSpec = {
  fixedColCount: number;
  fixedRowCount: number;
  scrollX: number;
  scrollY: number;
  firstVisibleColumn: number;
  firstVisibleRow: number;
  lastVisibleColumn: number;
  lastVisibleRow: number;
};

export type Dimensions = {
  width: number;
  height: number;
}