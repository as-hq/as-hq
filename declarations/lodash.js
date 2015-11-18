declare module lodash {
  declare function forEach<A,B>(
    obj: {[key: A]: B},
    cb: (v: B, k: A) => void
  ): void;
}
