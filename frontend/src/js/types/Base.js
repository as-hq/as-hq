/* @flow */

export type Callback<T> = (x: T) => void;

export type PairObject<T> = { fst: T; snd: T; };

export type Dict<A> = { [key: string]: A };

export type Lens<S, T> = {
  get: (state: S) => T;
  set: (state: S, val: T) => void;
};

export type IntervalId = number;

export type FileExportType =
    'Excel'
  | 'AlphaSheets'
  ;
