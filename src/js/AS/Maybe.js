/* @flow */

import type {
  Callback
} from '../types/Base';

class _Maybe<T>{
  _isJust: boolean;
  _val: ?T;

  constructor(isJust: boolean, val?: ?T) {
    this._isJust = isJust && (!!val);
    this._val = val;
  }

  bind<U>(func: (v: T) => Maybe<U>): _Maybe<U> {
    if (this._isJust) {
      if (! this._val) {
        throw new Error('Maybe invariant violation');
      }

      return func(this._val);
    } else {
      return Nothing();
    }
  }

  fmap<U>(func: (v: T) => ?U): _Maybe<U> {
    return this.bind((v) => Just(func(v)));
  }

  fmap_(func: (v: T) => void): void {
    this.bind((v) => {
      func(v);
      return Nothing();
    });
  }

  out(): ?T {
    return this._val;
  }
};

export type Maybe<T> = _Maybe<T>;

export function Just<T>(val: ?T): Maybe<T> {
  return (new _Maybe(true, val): Maybe<T>);
}

export function Nothing<T>(): Maybe<T> {
  return new _Maybe(false);
}

export function using<T>(a: ?T): Callback<Callback<T>> {
  return (cb: Callback<T>) => Just(a).fmap_(cb);
}

export function catMaybes<T>(arr: Array<?T>): Array<T> {
  let ret: Array<T> = [];
  arr.forEach((ele) => {
    if (ele !== null && ele !== undefined) {
      ret.push(ele);
    }
  });
  return ret;
}
