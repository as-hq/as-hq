import _ from 'underscore';
import fs from 'fs';
//import jasminePit from 'jasmine-pit';

//jasminePit.install(window);

jasmine.DEFAULT_TIMEOUT_INTERVAL = 10000;

describe('backend', () => {
  const API = require('../src/js/actions/ASApiActionCreators');
  const Converter = require('../src/js/AS/Converter');
  const Util = require('../src/js/AS/Util');
  const Store = require('../src/js/stores/ASEvaluationStore');

  function empty() {
    return new Promise((fulfill, reject) => { fulfill(); });
  }

  function locFromExcel(exLoc) {
    return Util.excelToLoc(exLoc);
  }

  function locToExcel(loc) {
    return Util.locToExcel(loc);
  }

  function fromToInclusive(st, end) {
    return _.range(end - st).map((i) => i + st);
  }

  //(a -> (), a -> ()) -> (() -> Promise a)
  function promise(fn) {
    return () => {
      return new Promise(fn);
    };
  }

  function exec(fn) {
    return promise((fulfill, reject) => {
      fn();
      fulfill();
    });
  }

  function apiExec(fn) {
    return promise((fulfill, reject) => {
      API.test(fn, {
        fulfill: fulfill,
        reject: reject
      });
    });
  }

  function apiSyncExec(fn) {
    return promise((fulfill, reject) => {
      API.testSync(fn, {
        fulfill: fulfill,
        reject: reject
      });
    });
  }

  function directAPIExec(fn) {
    return new Promise((fulfill, reject) => {
      API.test(fn, {
        fulfill: fulfill,
        reject: reject
      });
    });
  }

  function sheet() {
    // TODO
    return () => {
      directAPIExec(() => {
        API.sendCreateSheetMessage();
      }).then((response) => {
        let {
          payload: {
            contents: [
              { // tag: 'WorkbookSheet'
                wsName: workbookName,
                wsSheets: [
                  {
                    // tag: 'ASSheet'
                    sheetId
                    }
                  ]
                }
              ]
            }
          } = response;


      });
    };
  }

  // monadic log operation, String -> (() -> Promise ())
  function logP(str) {
    return promise((fulfill, reject) => {
      console.log((new Date()).getTime(), 'Log inside promise:', str);
      fulfill();
    });
  }

  function openSheet() {
    return apiSyncExec(() => {
      API.sendDefaultOpenMessage();
    });
  }

  function syncWindow() {
    return apiExec(() => {
      API.updateViewingWindow({
        range: { col: 0, row: 0, col2: 100, row2: 100 }
      });
    });
  }

  function clear() {
    return apiExec(() => {
      API.sendClearRequest();
    });
  }

  function init() {
    return apiExec(() => {
      API.sendInitialMessage();
    });
  }

  function cell(loc, xp, lang) {
    return apiExec(() => {
      let langMap = {
        'py': 'Python',
        'R': 'R'
      };
      let cell = Converter.clientToASCell(
        { range: locFromExcel(loc) },
        { exp: xp, lang: { Server: langMap[lang] } }
      );
      let msg = Converter.createEvalRequestFromASCell(cell);
      API.send(msg);
    });
  }

  function python(loc, xp) {
    return cell(loc, xp, 'py');
  }

  function r(loc, xp) {
    return cell(loc, xp, 'R');
  }

  function copy(rng1, rng2) {
    return apiExec(() => {
      API.sendCopyRequest([rng1, rng2].map(locFromExcel));
    });
  }

  function undo() {
    return apiExec(() => {
      API.sendUndoRequest();
    });
  }

  function redo() {
    return apiExec(() => {
      API.sendRedoRequest();
    });
  }

  function valueD(val) {
    return { tag: 'ValueD', contents: val };
  }

  function valueI(val) {
    return { tag: 'ValueI', contents: val };
  }

  function equalValues(val1, val2) {
    return _.isEqual(val1, val2);
  }

  function messageShouldSatisfy(loc, fn) {
    return promise((fulfill, reject) => {
      API.test(() => {
        API.sendGetRequest([ Util.excelToLoc(loc) ]);
      }, {
        fulfill: (result) => {
          let cs = Converter.clientCellsFromServerMessage(result);
          fn(cs);

          fulfill();
        },
        reject: reject
      });
    });
  }

  // String -> ASValue -> (() -> Promise ())
  function shouldBe(loc, val) {
    return messageShouldSatisfy(loc, (cs) => {
      console.log(`${loc} should be ${JSON.stringify(val)}`);

      expect(cs.length).not.toBe(0);
      if (cs.length == 0) {
        return;
      }

      let [{ cellValue }] = cs;
      expect(equalValues(cellValue, val)).toBe(true);
    });
  }

  function shouldBeNothing(loc) {
    return messageShouldSatisfy(loc, (cs) => {
      console.log(`${loc} should be nothing`);
      expect(cs.length).toBe(0);
    });
  }

  function shouldBeAnError(loc) {
    return messageShouldSatisfy(loc, (cs) => {

    });
  }

  // [String] -> [ASValue] -> (() -> Promise ())
  function shouldBeL(locs, vals) {
    return promise((fulfill, reject) => {
      API.test(() => {
        API.sendGetRequest(locs.map(Util.excelToLoc));
      }, {
        fulfill: (result) => {
          let cellValues = Converter.clientCellsFromServerMessage(result).map((x) => x.cellValue);
          expect(_.
            zip(cellValues, vals).
            map(([x, y]) => equalValues(x, y)).
            reduce((acc, cur) => {
              return acc && cur;
            }, true)
          ).toBe(true);

          fulfill();
        },
        reject: reject
      });
    });
  }

  // -- MONAD OPERATIONS

  // [() -> Promise a] -> Promise ()
  function _do(promiseFunctions, lbl) {
    let [head, ...tail] = promiseFunctions;

    if (!head) return empty;

    return head().then(_doDefer(tail), (failure) => {
      console.log('error in monad', lbl, failure);
    }).catch((error) => {
      console.log('promise error', error.toString());
    });
  }

  // [() -> Promise a] -> (() -> Promise ())
  function _doDefer(promises, lbl) {
    return () => {
      return _do(promises, lbl);
    };
  }

  // [a] -> (a -> (() -> Promise b)) -> (() -> Promise ())
  function _forM_(arr, pfn) {
    return _doDefer(arr.map(pfn));
  }

  describe('login', () => {
  });

  describe('crud', () => {
    beforeAll((done) => {
      _do([
        logP('Initializing...'),
        exec(done)
      ]);
    });

    it('clears a sheet', () => {
    });

    it('creates a new sheet', () => {
    });

    it('creates a new workbook', () => {
    });

    it('gets cells', () => {
    });
  });

  describe('dispatch', () => {
    beforeAll((done) => {
      _do([
        logP('Initializing...'),
        init(),
        logP('Opening sheet...'),
        openSheet(),
        logP('Syncing window...'),
        syncWindow(),
        logP('Set up environment.'),
        exec(done)
      ]);
    });

    beforeEach((done) => {
      _do([
        logP('Clearing sheet...'),
        clear(), // every it() starts with a clear spreadsheet
        logP('Finished preparing.'),
        logP('==========================STARTING TEST=========================='),
        exec(done)
      ]);
    });

    describe('eval', () => {
      describe('python', () => {
        it('should evaluate at all', (done) => {
          _do([
            python('A1', '1 + 1'),
            shouldBe('A1', valueI(2)),
            exec(done)
          ]);
        });

        it('should evaluate two cells, dependent', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('A2', 'A1 + 1'),
            shouldBe('A1', valueI(2)),
            shouldBe('A2', valueI(3)),
            exec(done)
          ]);
        });

        it('should evaluate a range and expand it', (done) => {
          _do([
            python('A1', 'range(10)'),
            _forM_(_.range(10), (i) => {
              return shouldBe(`A${i + 1}`, valueI(i));
            }),
            exec(done)
          ]);
        });

        xit('should evaluate to an error when there is one', (done) => {
          _do([
            python('A1', '1 + "a"'),
            //TODO
            exec(done)
          ]);
        });
      });

      describe('r', () => {
        it('should evaluate at all', (done) => {
          _do([
            r('A1', '1 + 1'),
            shouldBe('A1', valueI(2)),
            exec(done)
          ]);
        });

        it('should evaluate a range and expand it', (done) => {
          _do([
            r('A1', '1:10'),
            _forM_(_.range(10), (i) => {
              return shouldBe(`A${i + 1}`, valueI(i + 1));
            }),
            exec(done)
          ]);
        });
      });

      describe('general', () => {
        it('should do multi language eval', (done) => {
          _do([
            python('A1', '10'),
            r('B1', '1:A1'),
            _forM_(_.range(10), (i) => {
              return shouldBe(`B${i + 1}`, valueI(i + 1));
            }),
            exec(done)
          ]);
        });

        it('should shrink a range based on a dependency', (done) => {
          _do([
            python('A1', '10'),
            python('B1', 'range(A1)'),
            _forM_(_.range(10), (i) => {
              return shouldBe(`B${i + 1}`, valueI(i));
            }),
            python('A1', '1'),
            shouldBeNothing('B2'),
            exec(done)
          ]);
        });
      });
    });

    describe('repl eval', () => {
    });

    describe('cell transforms', () => {
      describe('copy/paste', () => {
        it('should copy and paste', (done) => {
          _do([
            python('A1', '1'),
            copy('A1', 'A2'),
            shouldBe('A2', valueI(1)),
            exec(done)
          ]);
        });

        it('should copy and paste a reference', (done) => {
          _do([
            python('A1', '1'),
            python('A2', '2'),
            python('B1', 'A1'),
            copy('B1', 'B2'),
            shouldBe('B2', valueI(2)),
            exec(done)
          ]);
        });

        it('should handle $A1 references', (done) => {
          _do([
            python('A1', '1'),
            python('B1', '$A1'),
            copy('B1', 'C1'),
            shouldBe('C1', valueI(1)),
            exec(done)
          ]);
        });

        it('should copy and paste a range reference', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('B1', '[x ** 2 for x in range(10)]'),
            python('C1', 'A1:A10.sum()'),
            copy('C1', 'D1'),
            shouldBe('D1', valueI(285)),
            exec(done)
          ]);
        });

        it('should tessellate a range', (done) => {
          let cs = {
            '00': 0,
            '10': 1,
            '01': 2,
            '11': 3
          };

          _do([
            python('A1', '[[0,1],[2,3]]'),
            copy('A1:B2', 'C1:F4'),
            _forM_(_.range(4), (col) => {
              return _forM_(_.range(4), (row) => {
                return shouldBe(
                  locToExcel({ col: col + 3, row: row + 1 }),
                  valueI(cs[`${col % 2}${row % 2}`])
                );
              });
            }),
            exec(done)
          ]);
        });

        it('should copy a cell down', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('B1', 'A1*2'),
            copy('B1', 'B2:B10'),
            _forM_(fromToInclusive(2, 10), (i) => {
              return shouldBe(`B${i}`, valueI((i-1) * 2));
            }),
            exec(done)
          ]);
        });

        it('should trigger eval when a cell is copied into a dependency', (done) => {
          _do([
            python('A1', '1'),
            python('B1', '2'),
            python('C1', 'B1 + 1'),
            shouldBe('C1', valueI(3)),
            copy('A1', 'B1'),
            shouldBe('C1', valueI(2)),
            exec(done)
          ]);
        });
      });
    });

    describe('vcs', () => {
      describe('undo', () => {
        it('should undo a simple request', (done) => {
          _do([
            python('A1', '10'),
            undo(),
            shouldBeNothing('A1'), // since cell should be clear
            exec(done)
          ]);
        });

        it('should undo a range request', (done) => {
          _do([
            python('A1', 'range(10)'),
            undo(),
            _forM_(_.range(10), (i) => {
              return shouldBeNothing(`A${i + 1}`);
            }),
            exec(done)
          ]);
        });

        xit('should undo a dependency cleanly', (done) => {
          // xcxc: test matters but blocks others. solved by finishing the asana task
          // for undo dealing with graphDB
          _do([
            python('A1', '1+1'),
            python('B1', 'A1+1'),
            python('C1', 'B1+1'),
            undo(),
            python('B1', '4'),
            shouldBe('B1', valueI(4)),
            shouldBeNothing('C1'),
            exec(done)
          ]);
        });
      });

      describe('redo', () => {
        it('should undo and redo a simple request', (done) => {
          _do([
            python('A1', '1 + 1'),
            undo(),
            redo(),
            shouldBe('A1', valueI(2)),
            exec(done)
          ]);
        });

        it('should undo and redo a range request', (done) => {
          _do([
            python('A1', 'range(10)'),
            undo(),
            redo(),
            _forM_(_.range(10), (i) => {
              return shouldBe(`A${i + 1}`, valueI(i));
            }),
            exec(done)
          ]);
        });

        it('should undo and redo series of dependencies', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            python('C1', 'A1 + B1'),
            undo(),
            redo(),
            undo(), undo(),
            redo(), redo(),
            undo(), undo(), undo(),
            redo(), redo(), redo(),
            shouldBe('A1', valueI(2)),
            shouldBe('B1', valueI(3)),
            shouldBe('C1', valueI(5)),
            exec(done)
          ]);
        });
      });
    });
  });

});
