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


let successCount = 0, testCount = 0;
let depth = 0;

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

function logSuccess(message) {
  console.log(`${tabs()}PASS: ${message}`);
}

function logFailure(message, error) {
  console.log(`${tabs()}FAIL: ${message}`);
  console.log(`${tabs()}__ERROR: ${error.toString()}`);
}

// (() -> Promise a) -> Test a
function _liftT(prf, message='', logs=false) {
  return promise((fulfill, reject) => {
    prf()
      .then(() => {
        if (logs) {
          logSuccess(message);
          successCount++;
        }

        fulfill();
      })
      .catch((error) => {
        if (logs) {
          logFailure(message, error);
        }

        reject(error);
      })
      .finally(() => {
        if (logs) {
          testCount++;
        }
      });
  });
}

function testInterpolate(tests, cbs) {
  let {
    beforeAll = empty,
    beforeEach = empty,
    afterAll = empty,
    afterEach = empty } =
    _.mapValues(cbs, _doDefer);

  return [
    beforeAll,
    tabbed(tests.map((test) =>
      _doDefer([beforeEach, test, afterEach])
    )),
    afterAll,
    logP(`${successCount} tests passed of ${testCount}`)
  ];
}


let hooks = {
  toBe(selfValue) {
    return {
      message(otherVal) {
        return `Expected ${JSON.stringify(selfValue)} to be ${JSON.stringify(otherVal)}`;
      },
      compare(otherVal) {
        return selfValue === otherVal;
      }
    };
  }
};

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
          throw new AssertionError(message(other));
        }
      }
    });
  }
}

export function expect(val) {
  return new Expect(val);
}

export function registerExpectation(name, ofs) {
  hooks[name] = ofs;
}

export function _it(message, prfs) {
  return _liftT(_doDefer(prfs), message, true);
}

export function _describe(name, {tests, ...cbs}) { // tests are either more describes or its
  return _liftT(_doDefer(testInterpolate(tests, cbs)), name);
}
