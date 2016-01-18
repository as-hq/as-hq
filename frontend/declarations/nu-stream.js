declare class NuStream<T> {
}

declare module 'nu-stream' {
  declare var exports: ({
    stream: ({
      toArray: <T>(stream: NuStream<T>) => Array<T>;
    });
  });
}
