/* @flow */

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
