/* @flow */

import type {
  Callback
} from '../types/Base';

import type {
  Prf,
  PromiseCallbacks
} from '../types/Tests';

import {logInfo} from '../AS/Logger';

import _ from 'lodash';
// import Promise from 'bluebird';

export function empty(): Promise {
  return new Promise((fulfill, reject) => { fulfill(); });
}

export function fromToInclusive(st: number, end: number): Array<number> {
  return _.range(end - st + 1).map((i) => i + st);
}

//(a -> (), a -> ()) -> (() -> Promise a)
export function promise(fn: PromiseCallbacks): Prf {
  return () => {
    return new Promise(fn);
  };
}

export function exec(fn: () => void): Prf {
  return promise((fulfill, reject) => {
    fn();
    fulfill();
  });
}

// monadic log operation, String -> (() -> Promise ())
export function logP(str: string): Prf {
  return promise((fulfill, reject) => {
    logInfo((new Date()).getTime().toString() + ' Log inside promise: ' + str);
    fulfill();
  });
}


export function blockUntil(
  fn: () => boolean,
  _interval: number = 100,
  _timeout: number = 5000
): Prf {
  return promise((fulfill, reject) => {
    let t = _timeout;

    /* following must be var, because it is self-referential and scoping will kill it */
    var tmr = setInterval(() => {
      if (fn()) {
        clearInterval(tmr);
        fulfill();
      } else if (t === _timeout % _interval) {
        clearInterval(tmr);
        reject('Timed out of blockUntil');
      }
      t -= _interval;
    }, _interval);
  });
}

export function blockUntilReady(fn: (cb: Callback) => void): Prf {
  return promise((fulfill, reject) => {
    fn(fulfill);
  });
}


// -- MONAD OPERATIONS

// [() -> Promise a] -> Promise ()
export function _do([head, ...tail]: Array<Prf>, lbl?: string): Promise {
  if (!head) return empty();

  return head().then(_doDefer(tail, lbl), (failure) => {
    throw new Error('Monadic error: ' + failure);
  }).catch((error) => {
    throw new Error(error);
  });
}

// [() -> Promise a] -> (() -> Promise ())
export function _doDefer(promises: Array<Prf>, lbl?: string): Prf {
  return () => {
    return _do(promises, lbl);
  };
}

// [a] -> (a -> (() -> Promise b)) -> (() -> Promise ())
export function _forM_<V>(arr: Array<V>, pfn: (v: V) => Prf): Prf {
  return _doDefer(arr.map(pfn));
}
