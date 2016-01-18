/* @flow */

import type {
  Dict
} from '../types/Base';

import stringify from 'json-stable-stringify';

export default class ObjectDict<A, B> {
  _dict: Dict<B>;

  constructor() { this._dict = {}; }

  get(item: A): B { return this._dict[stringify(item)]; }
  set(item: A, val: B) { this._dict[stringify(item)] = val; }
  del(item: A) {
    const str = stringify(item);
    if (this._dict[str] != null) {
      delete this._dict[stringify(item)];
    }
  }
}
