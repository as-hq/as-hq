declare module lodash {
  declare function forEach<A,B>(
    obj: {[key: A]: B},
    cb: (v: B, k: A) => void
  ): void;
  declare function isEqual(a: any, b: any): boolean;
  declare function range(a: number): Array<number>;
  declare function zip<A,B>(a: Array<A>, b: Array<B>): Array<[A,B]>;
}
