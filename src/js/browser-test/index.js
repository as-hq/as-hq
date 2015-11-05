import {setTestMode, unsetTestMode} from '../AS/Logger';

import _ from 'lodash';

import {
  addError,
  expect,
  registerExpectation,
  _describe,
  __describe,
  _it,
  _xdescribe,
  _xit
} from './test-framework';
import {
  promise,
  exec,
  logP,
  fromToInclusive,
  _do,
  _doDefer,
  _forM_,
  blockUntil,
  blockUntilReady
} from './exec-monad';
import {
  __injectExpect,

  rangeFromExcel,
  numToAlpha,

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
  shouldBeNothing,
  shouldBeL,

  actionAPIResponse,

  setUITestMode,
  unsetUITestMode
} from './exec-api';

import ASEvaluationStore from '../stores/ASEvaluationStore';
import Util from '../AS/Util';
import KeyUtils from '../AS/KeyUtils';

// import Promise from 'bluebird';

let evalPane;

function spreadsheet() {
  return evalPane.refs.spreadsheet;
}

function hypergrid() {
  return spreadsheet()._getHypergrid();
}

function activeSelection() {
  return ASEvaluationStore.getActiveSelection();
}

function activeRange() {
  return activeSelection().range;
}

function viewingWindow() {
  return spreadsheet().getViewingWindow().range;
}

function generateKeyEvent(key) {
  let {keyCode, ...keyEvent} = KeyUtils.parseIntoShortcut({}, key);
  let patchedKeyEvent = {
    ctrlKey: false,
    shiftKey: false,
    altKey: false,
    metaKey: false,
    ...keyEvent
  };

  return {
    persist() {},
    preventDefault() {},
    stopPropagation() {},
    which: keyCode,
    ...patchedKeyEvent
  };
}

function keyPress(key) {
  let evt = generateKeyEvent(key);
  return exec(() => { spreadsheet()._onKeyDown(evt) });
}

function mKeyPress(key) {
  return () => keyPress(key);
}

function keyPresses(str) {
  return _forM_(str.split(''), keyPress);
}

let [ pressUndo ] =
  [ 'Ctrl+Z' ].map(mKeyPress); /* future key shortcuts go here, for example Ctrl+Z */

function pressCopy() {
  return exec(() => {
    evalPane.handleCopyTypeEventForGrid({
      preventDefault() { },
      stopPropagation() { },
      clipboardData: {
        setData() { },
      }
    }, false);
  });
}

function pressCut() {
  return exec(() => {
    evalPane.handleCopyTypeEventForGrid({
      preventDefault() { },
      stopPropagation() { },
      clipboardData: {
        setData() { },
      }
    }, true);
  });
}

function pressPaste() {
  return exec(() => {
    evalPane.handlePasteEventForGrid({
      preventDefault() { },
      stopPropagation() { },
      clipboardData: {
        getData(x) { return ''; },
        types: []
      }
    });
  });
}

function strictRange(excelRng) {
  return excelRng.includes(':') ? excelRng : `${excelRng}:${excelRng}`;
}

function selectRange(excelRng, excelOrigin='') {
  excelRng = strictRange(excelRng);
  if (excelOrigin === '') {
    [excelOrigin] = excelRng.split(':');
  }

  return exec(() => {
    let hgRange = Util.excelToRange(excelRng),
        hgOrigin = Util.excelToRange(excelOrigin);

    // shouldn't scroll, so second parameter is false
    // this is to prevent updateViewingWindow from interfering with callbacks
    // and WS responses desyncing from actions

    spreadsheet().select({range: hgRange, origin: hgOrigin.tl}, false);
  });
}

function focusOnComponent(comp) {
  return exec(() => {
    comp.getDOMNode().focus();
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
  rng = strictRange(rng);
  return blockUntil(() => {
    return _.isEqual(clipboardRange(), rangeFromExcel(rng));
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

function shouldBeSelected(rng) {
  return exec(() => {
    rng = strictRange(rng);
    expect(activeRange()).toBeSupersetOf(rangeFromExcel(rng));
  });
}

function shouldHaveFocus(comp) {
  return exec(() => {
    let domNode = comp.getDOMNode();
    expect(domNode.hasFocus()).toBe(true, 'Expected component to have focus');
  });
}

function viewingWindowShouldSatisfy(fn) {
  return exec(() => {
    expect(viewingWindow()).toSatisfy(fn);
  });
}

function viewingWindowShouldBeAtRow(num) {
  return viewingWindowShouldSatisfy(({br: {row: bottomRow}}) => {
    return (bottomRow - 5 <= num) && (bottomRow + 5 >= num);
  });
}

function waitForResponse(act) {
  return promise((fulfill, reject) => {
    actionAPIResponse(act, fulfill)();
  });
}

let hooks = {

};


let tests = __describe('keyboard tests', {
  beforeAll: [ // prfs
    exec(() => {
      evalPane.enableTestMode();
      setUITestMode();
      setTestMode();
    })
  ],

  afterAll: [
    exec(() => {
      evalPane.disableTestMode();
      unsetUITestMode();
      unsetTestMode();
    })
  ],

  beforeEach: [
    clear()
  ],

  tests: [
    _describe('eval', { tests: [
      _it('should eval on enter', [
        selectRange('A1'),
        keyPresses('123'),
        waitForResponse(
          keyPress('Enter')
        ),
        shouldBe('A1', valueI(123)),
        shouldBeSelected('A2')
      ]),

      _it('should eval on tab', [
        selectRange('A1'),
        keyPresses('1234'),
        waitForResponse(
          keyPress('Tab')
        ),
        shouldBe('A1', valueI(1234)),
        shouldBeSelected('B1')
      ])
    ]}),

    _describe('copy and paste', { tests: [
      _it('should copy a cell', [
        python('A1', '1'),
        selectRange('A1'),
        pressCopy(),
        blockUntilCopy('A1')
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
      ]),

      _describe('regressions', { tests: [
      ]})
    ]}),

    _describe('undo and redo', { tests: [
      _it('undoes a simple eval', [
        python('A1', '1'),
        waitForResponse(
          keyPress('Ctrl+Z')
        ),
        shouldBeNothing('B1')
      ])
    ]}),

    _describe('deletion', { tests: [
      _it('deletes a range', [
        python('A1', 'range(10)'),
        selectRange('A1:A10'),
        waitForResponse(
          keyPress('Backspace')
        ),
        _forM_(fromToInclusive(1, 10),
          (i) => shouldBeNothing(`A${i}`)
        )
      ])
    ]}),

    _describe('selection shortcuts', { tests: [
      _xdescribe('selecting cells with ctrl a', { tests: [
        _it('selects a simple table', [
          python('A1', 'range(10)'),
          selectRange('A1'),
          keyPress('Ctrl+A'),
          shouldBeSelected('A1:A10')
        ]),

        _it('selects a table with a part that sticks out', [
          python('A1', 'range(10)'),
          python('B1', '1'),
          selectRange('A1'),
          keyPress('Ctrl+A'),
          shouldBeSelected('A1:B10')
        ])
      ]}),

      _describe('selecting cells with ctrl arrow', { tests: [
        _describe('basic functionality', { tests: [
          _it('selects down', [
            python('A1', 'range(10)'),
            selectRange('A1'),
            keyPress('Ctrl+Shift+Down'),
            shouldBeSelected('A1:A10')
          ]),

          _it('selects up', [
            python('A1', 'range(10)'),
            selectRange('A5'),
            keyPress('Ctrl+Shift+Up'),
            shouldBeSelected('A1:A5')
          ]),

          _it('selects right', [
            _forM_(fromToInclusive(1, 5),
              (i) => python(`${numToAlpha(i - 1)}1`, `${i}`)
            ),
            selectRange('A1'),
            keyPress('Ctrl+Shift+Right'),
            shouldBeSelected('A1:E1')
          ]),

          _it('selects left', [
            _forM_(fromToInclusive(1, 5),
              (i) => python(`${numToAlpha(i - 1)}1`, `${i}`)
            ),
            selectRange('C1'),
            keyPress('Ctrl+Shift+Left'),
            shouldBeSelected('A1:C1')
          ])
        ]}),

        _xdescribe('screen should follow ctrl arrow', { tests: [
          _it('selects down', [
            python('A1', 'range(60)'),
            selectRange('A1'),
            keyPress('Ctrl+Shift+Down'),
            shouldBeSelected('A1:A60'),
            viewingWindowShouldBeAtRow(60)
          ]),

          _it('selects down a long range', [
            python('A1', 'range(100)'),
            selectRange('A1'),
            keyPress('Ctrl+Shift+Down'),
            shouldBeSelected('A1:A100'),
            viewingWindowShouldBeAtRow(100)
          ])
        ]})
      ]})
    ]}),

    _describe('replication shortcuts', { tests: [
      _it('duplicates cells with ctrl d', [
        python('A1', 'range(5)'),
        python('B1', 'A1 + 1'),
        selectRange('B1:B5'),
        waitForResponse(
          keyPress('Ctrl+D')
        ),
        shouldBeL(
          fromToInclusive(1, 5).map((i) => `B${i}`),
          fromToInclusive(1, 5).map(valueI)
        )
      ]),

      _it('duplicates cells with ctrl r', [
        _forM_(fromToInclusive(1, 5),
          (i) => python(`${numToAlpha(i - 1)}1`, `${i}`)
        ),
        python('A2', 'A1 + 1'),
        selectRange('A2:E2'),
        waitForResponse(
          keyPress('Ctrl+R')
        ),
        shouldBeL(
          fromToInclusive(1, 5).map((i) => `${numToAlpha(i - 1)}2`),
          fromToInclusive(2, 6).map(valueI)
        )
      ])
    ]})
  ]
});

export function install(w, ep) {
  evalPane = ep;
  w.test = () => { tests(); };
  w.onerror = (msg, url, lineNumber) => {
    addError(msg);
    return true;
  };
  __injectExpect(expect);

  /* only do this with bluebird */
  // Promise.config({ longStackTraces: true, warnings: true });

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


