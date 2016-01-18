declare module lodash {
  declare type NestedArrayEntry<T> = T | Array<T>;
  declare type NestedArray<T> = Array<NestedArrayEntry<T>>;

  declare function cloneDeep<T>(a: T): T;
  declare function extend<A,B>(a: A, b: B): (A&B);
  declare function invert<A,B>(x: {[key: A]: B}): {[key: B]: A};
  declare function forEach<A,B>(
    obj: {[key: A]: B},
    cb: (v: B, k: A) => void
  ): void;
  declare function flatten<A>(arr: NestedArray<A>): Array<A>;
  // NOTE: the following function isn't actually of this type signature,
  // but it seems good to annotate it this way to catch more bugs, since
  // it's usually used to gauge whether two things of the same type are equal.
  declare function isEqual<A>(a: A, b: A): boolean;
  declare function range(a: number): Array<number>;
  declare function zip<A,B>(a: Array<A>, b: Array<B>): Array<[A,B]>;
}
