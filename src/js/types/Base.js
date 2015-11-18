/* @flow */

export type Callback<T> = (x: T) => void;
export type PairObject<T> = { fst: T; snd: T; };
export type Dict<A> = { [key: string]: A };
export type IntervalId = number;
