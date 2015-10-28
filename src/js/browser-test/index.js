import _ from 'lodash';

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
  locFromExcel,

  python,
  r,
  ocaml,
  excel,

  clear,
  cut,
  copy,
  undo,
  redo
} from './exec-api';

import ASEvaluationStore from '../stores/ASEvaluationStore';
import Util from '../AS/Util';

let evalPane;

function hypergrid() {
  return evalPane.refs.spreadsheet._getHypergrid();
}

function generateKeyEventDetail(key) {
  // TODO

  return {
    which: undefined,
    shiftKey: undefined
  };
}

function keyPress(key) {
  hypergrid().fireSyntheticKeydownEvent({
    detail: generateKeyEventDetail(key)
  });
}

function mKeyPress(key) {
  return () => exec(() => { keyPress(key); });
}

let [ pressLeft, pressRight, pressCopy, pressPaste ] =
  [ 'Left', 'Right', 'Ctrl+C', 'Ctrl+V' ].map(mKeyPress);

function selectRange(excelRng) {
  return exec(() => {
  });

  // THIS IS THE WRONG WAY TO SELECT RANGE, THIS SETS THE CLIPBOARD
}

function clipboardRange() {
  return ASEvaluationStore.getClipboard().area;
}

function formatTestCellToStore() {
  // TODO
}

/* block until the range registers the new selection */
function blockUntilCopy(rng) {
  return blockUntil(() =>
      _.isEqual(clipboardRange(), locFromExcel(rng))
  );
}

function blockOnGetCells(cs) {
  return blockUntil(() =>
      _.isEqual(
        ASEvaluationStore.getLastUpdatedCells(),
        cs.map(formatTestCellToStore)
      )
  );
}

function shouldReceiveResponse(resp) {
  return (result) => (
    _.isEqual(resp, result)
  );
}


let tests = _describe('keyboard tests', {
  beforeAll: [ // prfs
    logP('Initializing tests...') // no actual need to init
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
      ])
    ]})
  ]
});

export function install(w, ep) {
  evalPane = ep;
  w.test = tests;

  /* needed for test success count */
  Promise.prototype.finally = function (callback) {
    return this.then(
        value => this.constructor.resolve(callback()).then(() => value),
        reason => this.constructor.resolve(callback()).then(() => { throw reason; })
    );
  };
}


