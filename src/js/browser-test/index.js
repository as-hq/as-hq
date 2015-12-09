import {setTestMode, unsetTestMode} from '../AS/Logger';

import _ from 'lodash';

import {
  addError,
  getHooks,
  expect,
  _expect,
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
  decouple,

  shouldBe,
  shouldBeNothing,
  shouldBeL,

  actionAPIResponse,
  waitForResponse,

  setUITestMode,
  unsetUITestMode
} from './exec-api';

import ASEvaluationStore from '../stores/ASEvaluationStore';
import ASExpStore from '../stores/ASExpStore';
import Util from '../AS/Util';
import KeyUtils from '../AS/KeyUtils';
import ShortcutUtils from '../AS/ShortcutUtils';

// import Promise from 'bluebird';

let evalPane, window_, clipboard = {};

function spreadsheet() {
  return evalPane.refs.spreadsheet;
}

function hypergrid() {
  return spreadsheet()._getHypergrid();
}

function cellProvider() {
  return spreadsheet()._getBehavior().getCellProvider();
}

function textbox() {
  return $($('div#textbox.ace_editor.ace-tm>textarea')[0]);
}

function textboxHasFocus() {
  return textbox().is(':focus');
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

function expression() {
  return ASExpStore.getExpression();
}

function cellConfig(excelCell) {
  let {col, row} = Util.excelToIndex(excelCell);
  let [x, y] = [col - 1, row - 1];
  let config = { x: x, y: y };
  let provider = cellProvider();
  let {config: modifiedConfig} = provider.getCell(config);

  return modifiedConfig;
}

function fireKeyEventAtHypergrid(event) {
  hypergrid().fireSyntheticKeydownEvent(event);
  hypergrid().delegateKeyDown(event);
}

function getDetailOf(keyCode, shiftKey) {
  let details = {
    37: 'LEFT',
    38: 'UP',
    39: 'RIGHT',
    40: 'DOWN'
  };
  let detail = details[keyCode];

  if (!detail) return '';

  return `${detail}${shiftKey ? 'SHIFT' : ''}`;
}

function generateSyntheticBaseEvent() {
  let eventObj =
    document.createEventObject ? document.createEventObject : document.createEvent('Events');
  if (eventObj.initEvent) {
    eventObj.initEvent('keydown', true, true);
  }

  return eventObj;
}

function generateSyntheticKeyEvent(key) {
  let {keyCode, ...keyEvent} = ShortcutUtils.parseKeysIntoShortcut({}, key);
  let patchedKeyEvent = {
    ctrlKey: false,
    shiftKey: false,
    altKey: false,
    metaKey: false,
    ...keyEvent
  };

  let eventObj = generateSyntheticBaseEvent();
  Object.assign(eventObj, {
    persist() {},
    detail: {
      char: getDetailOf(keyCode, patchedKeyEvent.shiftKey)
    },
    which: keyCode,
    ...patchedKeyEvent
  });

  return eventObj;
}

function keyPress(key) {
  return exec(() => {
    let ev = generateSyntheticKeyEvent(key);
    if (textboxHasFocus()) {

    }
    fireKeyEventAtHypergrid(ev);
    spreadsheet()._onKeyDown(ev);
  });
}

function mKeyPress(key) {
  return () => keyPress(key);
}

function keyPresses(str) {
  return _forM_(str.split(''), keyPress);
}

let [ pressUndo, up, down, left, right, enter, tab, plus, backspace ] =
  [ 'Ctrl+Z', 'Up', 'Down', 'Left', 'Right', 'Enter', 'Tab', 'Shift+=', 'Backspace' ].map(mKeyPress); /* future key shortcuts go here, for example Ctrl+Z */

function pressCopy() {
  return exec(() => {
    evalPane.handleCopyTypeEventForGrid({
      preventDefault() { },
      stopPropagation() { },
      clipboardData: {
        setData(textType, val) {
          clipboard[textType] = "<meta>" + val; // for chrome
        }
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
        setData(textType, val) {
          clipboard[textType] = val;
        }
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
        getData(textType) { return clipboard[textType]; },
        types: []
      }
    });
  });
}

function doubleClick() {
  return exec(() => {
    hypergrid().canvas.dispatchEvent(new CustomEvent('fin-double-click', { detail: {} }));
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

    spreadsheet().select({range: hgRange, origin: hgOrigin.tl});
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

function atRow(num) {
  return ({br: {row: bottomRow}}) => {
    return (bottomRow - 5 <= num) && (bottomRow + 5 >= num);
  }
}

let hooks = {
  toBeSelected(rng) { // here, expect(rng).toBeSelected() is correct notation
    return {
      message() { return `Expected ${rng} to be selected.`; },
      compare() {
        return getHooks().toBeSupersetOf(activeRange()).compare(rangeFromExcel(rng));
      }
    }
  },

  toHaveFocus(elementFn) {
    return {
      message() { return `Expected ${elementFn()} to have focus.`; },
      compare() {
        let element = elementFn();
        if (element.hasFocus) return element.hasFocus();
        else return element.is(':focus');
      }
    }
  },

  toBeColored(cell) {
    return {
      message() { return `Expected ${cell} to be colored.`; },
      compare() {
        return (({bgColor}) => bgColor != undefined)(cellConfig(cell));
      }
    }
  },

  _toBe(cell) {
    return {
      isPromise: true,
      compare(value) {
        return shouldBe(cell, value);
      }
    }
  },

  _toBeNothing(cell) {
    return {
      isPromise: true,
      compare() {
        return shouldBeNothing(cell);
      }
    }
  }
};

function enableTestContext() {
  evalPane.enableTestMode();
  setTestMode();
  setUITestMode();
  window_.onerror = (msg, url, lineNumber) => {
    addError(msg);
    return true;
  };
}

function disableTestContext() {
  evalPane.disableTestMode();
  unsetTestMode();
  unsetUITestMode();
  window_.onerror = (msg) => {
    return false;
  };
}

/* wrapping tests in a closure was necessary for _expect to add dynamic hooks */
let tests = () => {
  return __describe('keyboard tests', {
    beforeAll: [ // prfs
      exec(enableTestContext)
    ],

    afterAll: [
      exec(disableTestContext)
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
            enter()
          ),
          _expect('A1')._toBe(valueI(123)),
          _expect('A2').toBeSelected()
        ]),

        _it('should eval on tab', [
          selectRange('A1'),
          keyPresses('1234'),
          waitForResponse(
            tab()
          ),
          _expect('A1')._toBe(valueI(1234)),
          _expect('B1').toBeSelected()
        ])
      ]}),

      _describe('textbox', { tests: [
        _describe('eval on arrows', { tests: [
          _it('evals on down', [
            selectRange('A1'),
            keyPresses('123'),
            waitForResponse(
              down()
            ),
            _expect('A1')._toBe(valueI(123))
          ])
        ]}),

        _describe('cell ref click/keyboard injection', { tests: [
          _it('injects cell ref from selection change', [
            _it('injects from arbitrary selection', [
              selectRange('A1'),
              keyPresses('=123'),
              plus(),
              selectRange('B1'),
              _expect(expression).toCurrentlyBe('=123+B1'),
              waitForResponse(
                enter()
              )
            ])
          ]),

          _describe('injects cell ref from pressing keyboard buttons', { tests: [
            _it('injects from down', [
              selectRange('A1'),
              keyPresses('=123'),
              plus(),
              down(),
              _expect(expression).toCurrentlyBe('=123+A2'),
              waitForResponse(
                enter()
              )
            ]),

            _it('injects from up', [
              selectRange('A2'),
              keyPresses('=123'),
              plus(),
              up(),
              _expect(expression).toCurrentlyBe('=123+A1'),
              waitForResponse(
                enter()
              )
            ]),

            _it('injects from left', [
              selectRange('B1'),
              keyPresses('=123'),
              plus(),
              left(),
              _expect(expression).toCurrentlyBe('=123+A1'),
              waitForResponse(
                enter()
              )
            ]),

            _it('injects from right', [
              selectRange('A1'),
              keyPresses('=123'),
              plus(),
              right(),
              _expect(expression).toCurrentlyBe('=123+B1'),
              waitForResponse(
                enter()
              )
            ])
          ]})
        ]}),

        _describe('f2 moves focus', { tests: [
          _it('moves focus from the spreadsheet to the textbox', [
            selectRange('A1'),
            keyPresses('=1'),
            waitForResponse(
              enter()
            ),
            _expect('A1')._toBe(valueI(1)),
            selectRange('A1'),
            keyPress('F2'),
            _expect(textbox).toHaveFocus()
          ]),

          _it('moves focus from the textbox back to the spreadsheet', [
            selectRange('A1'),
            keyPress('F2'),
            keyPress('F2'),
            down(),
            _expect('A2').toBeSelected()
          ])
        ]}),

        _it('should overwrite in the textbox upon ctrl-A', [
          selectRange('A1'),
          keyPresses('123'),
          keyPress('Ctrl+A'),
          keyPress('1'),
          _expect(expression).toCurrentlyBe('1'),
          waitForResponse(
            enter() //pristine state for next test
          )
        ]),

        _it('focuses on textbox on double click', [
          selectRange('A1'),
          doubleClick(),
          _expect(textbox).toHaveFocus()
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
          _expect('B1')._toBe(valueI(1))
        ]),

        _describe('regressions', { tests: [
        ]})
      ]}),

      _describe('undo and redo', { tests: [
        _it('undoes a simple eval', [
          python('A1', '1'),
          waitForResponse(
            pressUndo()
          ),
          _expect('B1')._toBeNothing()
        ])
      ]}),

      _describe('deletion', { tests: [
        _it('deletes a range', [
          python('A1', 'range(10)'),
          selectRange('A1:A10'),
          waitForResponse(
            backspace()
          ),
          _forM_(fromToInclusive(1, 10),
            (i) => _expect(`A${i}`)._toBeNothing()
          )
        ]),

        _describe('regressions', { tests: [
          _describe('undoes range(10)', { tests: [
            _it('deletes the expression', [
              python('A1', 'range(10)'),
              waitForResponse(
                pressUndo()
              ),
              selectRange('A5'),
              _expect(expression).toCurrentlyBe('')
            ]),

            _it('deletes the list cell from the renderer', { tests: [
              python('A1', 'range(10)'),
              waitForResponse(
                pressUndo()
              ),
              _expect('A5').not.toBeColored()
            ]})
          ]})
        ]})
      ]}),

      _describe('selection shortcuts', { tests: [
        _xdescribe('selecting cells with ctrl a', { tests: [
          _it('selects a simple table', [
            python('A1', 'range(10)'),
            selectRange('A1'),
            keyPress('Ctrl+A'),
            _expect('A1:A10').toBeSelected()
          ]),

          _it('selects a table with a part that sticks out', [
            python('A1', 'range(10)'),
            python('B1', '1'),
            selectRange('A1'),
            keyPress('Ctrl+A'),
            _expect('A1:B10').toBeSelected()
          ])
        ]}),

        _describe('selecting cells with ctrl arrow', { tests: [
          _describe('basic functionality', { tests: [
            _it('selects down', [
              python('A1', 'range(10)'),
              selectRange('A1'),
              keyPress('Ctrl+Shift+Down'),
              _expect('A1:A10').toBeSelected()
            ]),

            _it('selects up', [
              python('A1', 'range(10)'),
              selectRange('A5'),
              keyPress('Ctrl+Shift+Up'),
              _expect('A1:A5').toBeSelected()
            ]),

            _it('selects right', [
              _forM_(fromToInclusive(1, 5),
                (i) => python(`${numToAlpha(i - 1)}1`, `${i}`)
              ),
              selectRange('A1'),
              keyPress('Ctrl+Shift+Right'),
              _expect('A1:E1').toBeSelected()
            ]),

            _it('selects left', [
              _forM_(fromToInclusive(1, 5),
                (i) => python(`${numToAlpha(i - 1)}1`, `${i}`)
              ),
              selectRange('C1'),
              keyPress('Ctrl+Shift+Left'),
              _expect('A1:C1').toBeSelected()
            ])
          ]}),

          _xdescribe('screen should follow ctrl arrow', { tests: [
            _it('selects down', [
              python('A1', 'range(60)'),
              selectRange('A1'),
              keyPress('Ctrl+Shift+Down'),
              _expect('A1:A60').toBeSelected(),
              _expect(viewingWindow).toCurrentlySatisfy(atRow(60))
            ]),

            _it('selects down a long range', [
              python('A1', 'range(100)'),
              selectRange('A1'),
              keyPress('Ctrl+Shift+Down'),
              _expect('A1:A100').toBeSelected(),
              _expect(viewingWindow).toCurrentlySatisfy(atRow(100))
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
      ]}),

      _describe('mac compatibility', { tests: [
        _it('does most shortcuts with cmd instead of ctrl', [
          python('A1', 'range(5)'),
          python('B1', 'A1 + 1'),
          selectRange('B1:B5'),
          waitForResponse(
            keyPress('Cmd+D')
          ),
          shouldBeL(
            fromToInclusive(1, 5).map((i) => `B${i}`),
            fromToInclusive(1, 5).map(valueI)
          )
        ]),

        _it('mac copy', [
          python('A1', '1'),
          selectRange('A1'),
          keyPress('Cmd+C'),
          blockUntilCopy('A1')
        ]),

        _it('mac copy/paste', [
          python('A1', '1'),
          selectRange('A1'),
          keyPress('Cmd+C'),
          blockUntilCopy('A1'),
          selectRange('B1'),
          waitForResponse(
            keyPress('Cmd+V')
          ),
          _expect('B1')._toBe(valueI(1))
        ]),

        _it('mac cut/paste', [
          python('A1', '1'),
          selectRange('A1'),
          keyPress('Cmd+X'),
          blockUntilCopy('A1'),
          selectRange('B1'),
          waitForResponse(
            keyPress('Cmd+V')
          ),
          _expect('A1')._toBeNothing(),
          _expect('B1')._toBe(valueI(1))
        ])
      ]})
    ]
  });
};

export function install(w, ep) {
  [evalPane, window_] = [ep, w];
  w.test = () => { tests()(); };
  __injectExpect(expect);

  w.$as = {};
  w.$as.ShortcutUtils = ShortcutUtils;
  w.$as.KeyUtils = KeyUtils;

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
