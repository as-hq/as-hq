import {logInfo, logGreen, logRed} from '../AS/Logger';

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

function tabify(message) {
  return `${tabs()}${message}`;
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
  });
}

function getSuccessCount() { return successCount; }

function getTestCount() { return testCount; }

function runTestsWithCallbacks(tests, cbs) {
  let {
    beforeAll = empty,
    beforeEach = empty,
    afterAll = empty,
    afterEach = empty } =
    _.mapValues(cbs, _doDefer);

  return _doDefer([
    beforeAll,
    tabbed(tests.map((test) =>
      _doDefer([
        beforeEach, test, afterEach
      ])
    )),
    afterAll
  ]);
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
  },

  toBeSupersetOf(selfValue) {
    return {
      message(otherVal) {
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
          assertionFails.push(message(other));
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
  return _liftT(_doDefer([
    exec(() => { logInfo(tabify(name)) }),
    runTestsWithCallbacks(tests, cbs, name)
  ]));
}

export function __describe(name, {tests, ...cbs}) {
  return _liftT(_doDefer([
    exec(() => { logInfo(tabify(name)) }),
    runTestsWithCallbacks(tests, cbs, name),
    exec(() => {
      logInfo(tabify(`${getSuccessCount()} tests passed of ${getTestCount()}`));
    })
  ]));
}
