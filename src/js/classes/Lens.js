/* @flow */

export default class Lens<A, B> { // extracts a B from an A
  _getter: (base: A) => B;
  _setter: (base: A, newVal: B) => void;
}
