declare module lodash {
  declare function cloneDeep<T>(a: T): T;
  declare function extend<A,B>(a: A, b: B): (A&B);
  declare function invert<A,B>(x: {[key: A]: B}): {[key: B]: A};
  declare function forEach<A,B>(
    obj: {[key: A]: B},
    cb: (v: B, k: A) => void
  ): void;
  declare function isEqual(a: any, b: any): boolean;
  declare function range(a: number): Array<number>;
  declare function zip<A,B>(a: Array<A>, b: Array<B>): Array<[A,B]>;
}
