/* @flow */

import type {Dict} from './Base';

export type ASTestLanguage = 'py' | 'R' | 'excel' | 'ml';

export type Prf<V> = () => Promise<V>;

export type PromiseCallbacks<V> =
  (
    fulfill: (result: Promise<V>|V) => void,
    reject: (error: any) => void
  ) => V;

export type Matcher<V> = Dict<((v: V) => boolean)>;
