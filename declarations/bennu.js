declare class Parser<T> {
}

declare module bennu {
  declare var exports: ({
    parse: {
      always: <A>(val: A) => Parser<A>; // equivalent of return
      bind: <A, B>(pa: Parser<A>, fb: (val: A) => Parser<B>) => Parser<B>;
      optional: <A, B>(val: A, pb: Parser<B>) => Parser<A | B>;
      many1: <A>(pa: Parser<A>) => Parser<Array<A>>;

      run: <A>(pa: Parser<A>, str: string) => A;
    };

    text: {
      character: (chr: string) => Parser<string>;
      letter: Parser<string>;
      digit: Parser<string>;
    };
  });
}
