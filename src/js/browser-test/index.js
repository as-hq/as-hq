import {expect, registerExpectation, _describe, _it} from './test-framework';
import {
  exec,
  logP,
  _do,
  _doDefer,
  _forM_,
  blockUntil,
  blockUntilReady
} from './exec-monad';
import {

} from './exec-api';

let evalPane;

let tests = _describe('test', {
  beforeAll: [ // prfs
  ],

  beforeEach: [
  ],

  tests: [
    _it('should do something', [
      exec(() => { expect(1).toBe(2); })
    ])
  ]
});

export function install(w, ep) {
  evalPane = ep;
  w.test = tests;
}


