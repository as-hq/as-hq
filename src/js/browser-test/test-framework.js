import {logWarn, logInfo, logGreen, logRed} from '../AS/Logger';

import _ from 'lodash';

import AssertionError from './assertion-error';
import {
  _do,
  _doDefer,
  promise,
  logP,
  empty,
  exec
} from './exec-monad';


let successCount = 0, skipCount = 0, testCount = 0;
let depth = 0;
let beforeStack = [];
let afterStack = [];
let skipTests = 0;

function tabbed(prfs) {
  return _doDefer([
    exec(() => { depth++; }),
    _doDefer(prfs),
    exec(() => { depth--; })
  ]);
}

// type Test a = () -> Promise (Either Error a)
// output of test is logged to console

function tabs() {
  return _.range(depth).map(() => '__').join('');
}

function tabify(message) {
  return `${tabs()}${message}`;
}

function logSkip(message) {
  logWarn(tabify(`SKIP: ${message}`));
}

function logSuccess(message) {
  logGreen(tabify(`PASS: ${message}`));
}

function logFailure(message, errors) {
  logRed(tabify(`FAIL: ${message}`));
  errors.forEach((err) => {
    logRed(tabify(`__ERROR: ${err.toString()}`));
  });
}

// (() -> Promise a) -> Test a
function _liftT(prf, message='', logs=false) {
  return promise((fulfill, reject) => {
    if (logs) { testCount++; }

    if ((skipTests > 0) && logs) {
      logSkip(message);
      skipCount++;
      fulfill();
    } else {
      prf()
        .then(() => {
          if (logs) {
            if (assertionFails.length > 0) {
              logFailure(message, assertionFails);
              assertionFails = [];
            } else {
              logSuccess(message);
              successCount++;
            }
          }

          fulfill();
        })
        .catch((error) => {
          if (logs) {
            logFailure(message, [error]);
          }

          fulfill();
        });
    }
  });
}

function getSuccessCount() { return successCount; }

function getSkipCount() { return skipCount; }

function getTestCount() { return testCount; }

function wrapEach(prf, beforeEach, afterEach) {
  return _doDefer([
    exec(() => {
      beforeStack.push(beforeEach);
      afterStack.push(afterEach);
    }),
    prf,
    exec(() => {
      beforeStack.pop();
      afterStack.pop();
    })
  ])
}

function _doStack(srf) {
  return () => {
    let stack = srf();
    return _do(_.flatten(stack));
  };
}

function _doDeferWithSkip(action) {
  return _doDefer([
    exec(() => { skipTests++; }),
    action,
    exec(() => { skipTests--; })
  ]);
}

function _doDeferWithWrap(action) {
  return _doDefer([
    _doStack(() => { return beforeStack; }),
    action,
    _doStack(() => { return afterStack; })
  ]);
}

function runTestsWithCallbacks(tests, cbs) {
  let {
    beforeAll = empty,
    beforeEach = empty,
    afterAll = empty,
    afterEach = empty } =
    _.mapValues(cbs, _doDefer);

  return _doDefer([
    beforeAll,
    _doDeferWithWrap(
      wrapEach(tabbed(tests), beforeEach, afterEach)
    ),
    afterAll
  ]);
}


let hooks = {
  toBe(selfValue, msg) {
    return {
      message(otherVal) {
        if (msg != undefined) return msg;
        return `Expected ${JSON.stringify(selfValue)} to be ${JSON.stringify(otherVal)}`;
      },
      compare(otherVal) {
        return selfValue === otherVal;
      }
    };
  },

  toBeSupersetOf(selfValue, msg) {
    return {
      message(otherVal) {
        if (msg != undefined) return msg;
        return `Expected ${JSON.stringify(selfValue)} to be superset of ${JSON.stringify(otherVal)}`;
      },
      compare(otherVal) {
        return Object.keys(otherVal)
          .map((k) => _.isEqual(otherVal[k], selfValue[k]))
          .reduce((acc, cur) => acc && cur, true);
      }
    }
  }
};

let assertionFails = [
];

class Expect {
  constructor(val, not=false) {
    let self = this;
    this.value = val;
    this.isNot = not;

    if (!not)
      this.not = new Expect(val, true);

    _.forEach(hooks, (v, k) => {
      self[k] = (other) => {
        let {message, compare} = v(self.value);
        if (compare(other) != (! this.isNot)) {
          if (this.isNot) {
            assertionFails.push(`NOT: ${message(other)}`);
          } else {
            assertionFails.push(message(other));
          }
        }
      }
    });
  }
}

export function addError(msg) {
  assertionFails.push(msg);
}

export function expect(val) {
  return new Expect(val);
}

export function registerExpectation(name, ofs) {
  hooks[name] = ofs;
}

export function _it(message, prfs) {
  return _liftT(
    _doDeferWithWrap(
      _doDefer(prfs)
    ),
    message, true);
}

export function _xit(message, prfs) {
  return _doDeferWithSkip(_it(message, prfs));
}

export function _describe(name, {tests, ...cbs}) { // tests are either more describes or its
  return _liftT(_doDefer([
    exec(() => { logInfo(tabify(name)) }),
    runTestsWithCallbacks(tests, cbs, name)
  ]));
}

export function _xdescribe(name, args) {
  return _doDeferWithSkip(_describe(name, args));
}

export function __describe(name, args) {
  return _doDefer([
    logP('====================================BEGINNING TESTS===================================='),
    _describe(name, args),
    exec(() => {
      logGreen(tabify(`${getSuccessCount()} tests passed`));
      logWarn(tabify(`${getSkipCount()} tests skipped`));
      logRed(tabify(`${getTestCount() - getSkipCount() - getSuccessCount()} tests failed`));
      logInfo(tabify(`${getTestCount()} tests total`));
    })
  ]);
}
