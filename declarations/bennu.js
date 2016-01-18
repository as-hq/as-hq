declare class Parser<T> {
}

declare module bennu {
  declare var exports: ({
    parse: {
      always: <A>(val: A) => Parser<A>; // equivalent of return
      attempt: <A>(pa: Parser<A>) => Parser<A>;
      bind: <A, B>(pa: Parser<A>, fb: (val: A) => Parser<B>) => Parser<B>;
      either: <A, B>(pa: Parser<A>, pb: Parser<B>) => Parser<A | B>;
      optional: <A, B>(val: A, pb: Parser<B>) => Parser<A | B>;
      many1: <A>(pa: Parser<A>) => Parser<NuStream<A>>;
      next: <A, B>(pa: Parser<A>, pb: Parser<B>) => Parser<B>;

      run: <A>(pa: Parser<A>, str: string) => A;
    };

    text: {
      character: (chr: string) => Parser<string>;
      letter: Parser<string>;
      digit: Parser<string>;
      string: (str: string) => Parser<string>;
    };
  });
}
