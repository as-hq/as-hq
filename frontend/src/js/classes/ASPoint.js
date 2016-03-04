/* @flow */

import type {
  NakedIndex,
  Offset,
} from '../types/Eval';

export default class ASPoint {
  _x: number;
  _y: number;

  get x(): number { return this._x; }
  get y(): number { return this._y; }

  constructor({x, y}: NakedIndex): ASPoint {
    this._x = x;
    this._y = y;
  }

  shift({dX, dY}: Offset): ASPoint {
    return new ASPoint({
      x: this._x + dX,
      y: this._y + dY
    });
  }

  equals(other: ASPoint) {
    return (
      other.x === this._x &&
      other.y === this._y
    );
  }

  static defaultPoint(): ASPoint {
    return new ASPoint({x: 1, y: 1});
  }
}
