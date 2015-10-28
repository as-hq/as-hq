import _ from 'lodash';

export function empty() {
  return new Promise((fulfill, reject) => { fulfill(); });
}

export function fromToInclusive(st, end) {
  return _.range(end - st).map((i) => i + st);
}

//(a -> (), a -> ()) -> (() -> Promise a)
export function promise(fn) {
  return () => {
    return new Promise(fn);
  };
}

export function exec(fn) {
  return promise((fulfill, reject) => {
    fn();
    fulfill();
  });
}

// monadic log operation, String -> (() -> Promise ())
export function logP(str) {
  return promise((fulfill, reject) => {
    console.log((new Date()).getTime(), 'Log inside promise:', str);
    fulfill();
  });
}


export function blockUntil(fn, _interval, _timeout=5000) {
  return promise((fulfill, reject) => {
    let t = _timeout;
    var tmr = setInterval(() => {
      if (fn()) {
        clearInterval(tmr);
        fulfill();
      } else if (t < 0) {
        clearInterval(tmr);
      }
      t -= _interval;
    }, _interval);
  });
}

export function blockUntilReady(fn) {
  return promise((fulfill, reject) => {
    fn(fulfill);
  });
}


// -- MONAD OPERATIONS

// [() -> Promise a] -> Promise ()
export function _do(promiseFunctions, lbl) {
  let [head, ...tail] = promiseFunctions;

  if (!head) return empty();

  return head().then(_doDefer(tail), (failure) => {
    console.log('error in monad', lbl, failure);
    throw new Error(failure);
  }).catch((error) => {
    console.log('promise error', error.toString());
    console.trace();
  });
}

// [() -> Promise a] -> (() -> Promise ())
export function _doDefer(promises, lbl) {
  return () => {
    return _do(promises, lbl);
  };
}

// [a] -> (a -> (() -> Promise b)) -> (() -> Promise ())
export function _forM_(arr, pfn) {
  return _doDefer(arr.map(pfn));
}
