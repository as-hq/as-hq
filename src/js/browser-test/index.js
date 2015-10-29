import _ from 'lodash';

import {expect, registerExpectation, _describe, _it} from './test-framework';
import {
  promise,
  exec,
  logP,
  _do,
  _doDefer,
  _forM_,
  blockUntil,
  blockUntilReady
} from './exec-monad';
import {
  __injectExpect,

  locFromExcel,

  python,
  r,
  ocaml,
  excel,

  valueI,

  clear,
  cut,
  copy,
  undo,
  redo,

  shouldBe,

  actionAPIResponse
} from './exec-api';

import ASEvaluationStore from '../stores/ASEvaluationStore';
import Util from '../AS/Util';
import KeyUtils from '../AS/KeyUtils';

let evalPane;

function spreadsheet() {
  return evalPane.refs.spreadsheet;
}

function hypergrid() {
  return spreadsheet()._getHypergrid();
}

function generateKeyEvent(key) {
  let {keyCode, ...keyEvent} = KeyUtils.parseIntoShortcut({}, key);
  return {
    persist() {},
    which: keyCode,
    ...keyEvent
  };
}

function keyPress(key) {
  let evt = generateKeyEvent(key);
  spreadsheet().handleKeyDown(evt);
}

function mKeyPress(key) {
  return () => exec(() => { keyPress(key); });
}

let [ ] =
  [ ].map(mKeyPress); /* future key shortcuts go here, for example Ctrl+Z */

function pressCopy() {
  return exec(() => {
    evalPane.handleCopyTypeEventForGrid({
      clipboardData: {
        setData() { }
      }
    }, false);
  });
}

function pressPaste() {
  return exec(() => {
    evalPane.handlePasteEventForGrid({
      clipboardData: {
        getData(x) { return ''; },
        types: []
      }
    });
  });
}

function selectRange(excelRng) {
  return exec(() => {
    let hgRange = Util.excelToRange(excelRng);
    spreadsheet().select(hgRange);
  });
}

function clipboardRange() {
  return ASEvaluationStore.getClipboard().area.range;
}

function formatTestCellToStore() {
  // TODO
}

/* block until the range registers the new selection */
function blockUntilCopy(rng) {
  return blockUntil(() => {
    console.log(clipboardRange(), locFromExcel(rng));
    return _.isEqual(clipboardRange(), locFromExcel(rng));
  });
}

function blockOnGetCells(cs) {
  return blockUntil(() =>
      _.isEqual(
        ASEvaluationStore.getLastUpdatedCells(),
        cs.map(formatTestCellToStore)
      )
  );
}

function waitForResponse(act) {
  return promise((fulfill, reject) => {
    actionAPIResponse(act, fulfill)();
  });
}

let hooks = {

};


let tests = _describe('keyboard tests', {
  beforeAll: [ // prfs
    logP('Initializing tests...'),
    exec(() => { evalPane.enableTestMode(); })
  ],

  afterAll: [
    logP('Winding down...'),
    exec(() => { evalPane.disableTestMode(); })
  ],

  beforeEach: [
    logP('Clearing sheet...'),
    clear()
  ],

  tests: [
    _describe('copy and paste', { tests: [
      _it('should copy a cell', [
        python('A1', '1'),
        selectRange('A1'),
        pressCopy(),
        blockUntilCopy('A1') /* don't finish the test until it actually stores in clipboard */
      ]),

      _it('should copy and paste a cell', [
        python('A1', '1'),
        selectRange('A1'),
        pressCopy(),
        blockUntilCopy('A1'),
        selectRange('B1'),
        waitForResponse(
          pressPaste()
        ),
        shouldBe('B1', valueI(1))
      ])
    ]})
  ]
});

export function install(w, ep) {
  evalPane = ep;
  w.test = tests;
  __injectExpect(expect);

  /* needed for test success count */
  Promise.prototype.finally = function (callback) {
    return this.then(
        value => this.constructor.resolve(callback()).then(() => value),
        reason => this.constructor.resolve(callback()).then(() => { throw reason; })
    );
  };

  _.forEach(hooks, (v, k) => {
    registerExpectation(k, v);
  });
}


