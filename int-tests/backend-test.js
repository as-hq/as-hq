import _ from 'underscore';
import fs from 'fs';
//import jasminePit from 'jasmine-pit';

//jasminePit.install(window);

jasmine.DEFAULT_TIMEOUT_INTERVAL = 10000;

describe('backend', () => {
  const API = require('../src/js/actions/ASApiActionCreators');
  const Util = require('../src/js/AS/Util');
  const Store = require('../src/js/stores/ASEvaluationStore');
  const TC = require('../src/js/AS/TypeConversions');

  function empty() {
    return new Promise((fulfill, reject) => { fulfill(); });
  }

  function locFromExcel(exLoc) {
    return Util.excelToRange(exLoc);
  }

  function locToExcel(loc) {
    return Util.rangeToExcel(loc);
  }

  function asIndex(loc) {
    return TC.simpleToASIndex(Util.excelToIndex(loc));
  }

  function asRange(loc) {
    return TC.simpleToASRange(Util.excelToRange(loc));
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
    return () => {
      directAPIExec(() => {
        API.createSheet();
      }).then((response) => {
        let  [
              { // tag: 'WorkbookSheet'
                wsName: workbookName,
                wsSheets: [
                  {
                    // tag: 'ASSheet'
                    sheetId
                    }
                  ]
                }
              ] = response.payload.contents;
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
      API.openSheet();
    });
  }

  function syncWindow() {
    return apiExec(() => {
      let range = { tl: {col: 0, row: 0}, br: {col: 100, row: 100 }},
          vWindow = TC.rangeToASWindow(range);
      API.updateViewingWindow(vWindow);
    });
  }

  function clear() {
    return apiExec(() => {
      API.clear();
    });
  }

  function init() {
    return apiExec(() => {
      API.initialize();
    });
  }

  function cell(loc, xp, lang) {
    return apiExec(() => {
      let langMap = {
        'py': 'Python',
        'R': 'R',
        'excel': 'Excel',
        'ml': 'OCaml'
      };
      let idx = asIndex(loc);
      console.log("\n\nFUCk\n", idx);
      let xpObj = { expression: xp, language: { Server: langMap[lang] } };
      API.evaluate(idx, xpObj);
    });
  }

  function python(loc, xp) {
    return cell(loc, xp, 'py');
  }

  function r(loc, xp) {
    return cell(loc, xp, 'R');
  }

  function excel(loc, xp) {
    return cell(loc, xp, 'excel');
  }

  function ocaml(loc, xp) {
    return cell(loc, xp, 'ml');
  }

  function copy(rng1, rng2) {
    return apiExec(() => {
      let [asRng1, asRng2] = [rng1, rng2].map(asRange);
      API.copy(asRng1, asRng2);
    });
  }

  function repeat(rng, origin) {
    return apiExec(() => {
      let sel = {origin: locFromExcel(origin), range: locFromExcel(rng)}
      API.repeat(sel);
    });
  }

  function cut(rng1, rng2) {
    return apiExec(() => {
      let [asRng1, asRng2] = [rng1, rng2].map(asRange);
      API.cut(asRng1, asRng2);
    });
  }

  function undo() {
    return apiExec(() => {
      API.undo();
    });
  }

  function redo() {
    return apiExec(() => {
      API.redo();
    });
  }

  function delete_(rng) {
    return apiExec(() => {
      API.deleteRange(TC.simpleToASRange(locFromExcel(rng)));
    });
  }

  function valueD(val) {
    return { tag: 'ValueD', contents: val };
  }

  function valueI(val) {
    return { tag: 'ValueI', contents: val };
  }

  function valueB(val) {
    return { tag: 'ValueB', contents: val };
  }

  function valueS(val) {
    return { tag: 'ValueS', contents: val };
  }

  function equalValues(val1, val2) {
    return _.isEqual(val1, val2);
  }

  // (() -> Promise a) -> (a -> Bool) -> (() -> Promise ())
  function responseShouldSatisfy(prf, fn) {
    return promise((fulfill, reject) => {
      prf().then((result) => {
        expect(fn(result)).toBe(true);
        fulfill();
      }).catch((error) => {
        reject(error);
      });
    });
  }

  function shouldError(prf) {
    return responseShouldSatisfy(prf, ({ result: { tag } }) => tag === 'Failure');
  }

  function messageShouldSatisfy(loc, fn) {
    return promise((fulfill, reject) => {
      API.test(() => {
        API.getIndices([ asIndex(loc) ]);
      }, {
        fulfill: (result) => {
          let cs = result.payload.contents;
          fn(cs);

          fulfill();
        },
        reject: reject
      });
    });
  }

  function expressionShouldSatisfy(loc, fn) {
    return messageShouldSatisfy(loc, (cs) => {
      console.log(`${loc} expression should satisfy ${fn.toString()}`);

      expect(cs.length).not.toBe(0);
      if (cs.length == 0) {
        return;
      }

      let [{ cellExpression }] = cs;
      expect(fn(cellExpression)).toBe(true);
    });
  }

  function expressionShouldBe(loc, xp) {
    return expressionShouldSatisfy(loc, ({ expression }) => expression === xp);
  }

  function valueShouldSatisfy(loc, fn) {
    return messageShouldSatisfy(loc, (cs) => {
      console.log(`${loc} should satisfy ${fn.toString()}`);

      expect(cs.length).not.toBe(0);
      if (cs.length == 0) {
        return;
      }

      let [{ cellValue }] = cs;
      expect(fn(cellValue)).toBe(true);
    });
  }

  // String -> ASValue -> (() -> Promise ())
  function shouldBe(loc, val) {
    return valueShouldSatisfy(loc, (cv) => equalValues(cv, val));
  }

  function shouldBeError(loc) {
    return valueShouldSatisfy(loc, ({ tag }) => (tag === 'ValueError' || tag == 'ValueExcelError'));
  }

  function shouldBeImage(loc) {
    return valueShouldSatisfy(loc, ({ tag }) => (tag === 'ValueImage'));
  }

  function shouldBeNothing(loc) {
    return messageShouldSatisfy(loc, (cs) => {
      console.log(`${loc} should be nothing`);
      //server should return either nothing at the location or a blank cell
      let isEmpty = (cs.length == 0) || (cs[0].cellExpression.expression == "");
      expect(isEmpty).toBe(true);
    });
  }

  // [String] -> [ASValue] -> (() -> Promise ())
  function shouldBeL(locs, vals) {
    return promise((fulfill, reject) => {
      API.test(() => {
        API.getIndices(locs.map(asIndex));
      }, {
        fulfill: (result) => {
          let cellValues = result.payload.contents.map((x) => x.cellValue);

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
      console.trace();
      throw new Error(failure);
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

        it('should fail to evaluate a circular dependency', (done) => {
          _do([
            python('A1', '1+1'),
            python('B1', 'A1+1'),
            shouldError(
              python('A1', 'B1')
            ),
            shouldBe('A1', valueI(2)),
            exec(done)
          ]);
        });

        it('should fail to evaluate a circular dependency arising from a range cell', (done) => {
          _do([
            python('A5', '5'),
            python('C5', 'A5 + 10'),
            shouldError(
              python('A1', 'range(C5, C5 + 10)')
            ),
            exec(done)
          ]);
        });

        it('range dependencies get updated', (done) => {
          _do([
            python('A1', 'range(2)'),
            python('B2', 'A2 + 1'),
            python('A1', 'range(4,6)'),
            shouldBe('B2', valueI(6)),
            exec(done)
          ]);
        });

        it('sophisticated range dependencies work as expected', (done) => {
          _do([
            python('A1', 'range(102,110)'),
            python('C3', 'range(A3, A3+3)'),
            python('E3', 'range(A3, A3+4)'),
            python('A1', 'range(C3,E5)'),
            shouldBe('A1', valueI(104)),
            shouldError(
              python('A1', 'range(C3,E6)')
            ),
            python('E3', 'range(A3+C4-104,A3+C3-104+4)'),
            shouldBe('A1', valueI(104)),
            shouldError(
              python('A1', 'range(C3,E6)')
            ),
            exec(done)
          ]);
        });

        it('should evaluate to an error when there is one', (done) => {
          _do([
            python('A1', '1 + "a"'),
            shouldBeError('A1'),
            exec(done)
          ]);
        });

        xit('plots shit', (done) => {
          _do([
            python('A1', 'import matplotlib.pyplot as plt; plt.plot([1,2,3])'),
            shouldBeImage('A1'),
            python('A1', 'import matplotlib.pyplot as plt; plt.plot([1,2,3]); plt.show()'),
            shouldBeImage('A1'),
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

        it('should evaluate a double', (done) => {
          _do([
            r('A1', '1.23'),
            shouldBe('A1', valueD(1.23)),
            exec(done)
          ]);
        });

        it('should evaluate lists correctly', (done) => {
          _do([
            r('A1', 'list(a=1,b=2)'),
            r('B1', 'A1$a'),
            shouldBe('B1', valueI(1)),
            exec(done)
          ]);
        });

        it('should evaluate a symbol correctly', (done) => {
          _do([
            r('A1', 'as.symbol(123)'),
            shouldBe('A1', valueS('123')),
            exec(done)
          ]);
        });

        it('should evaluate list dependencies', (done) => {
          _do([
            r('A1', 'c(1,2,3,4)'),
            r('B1', 'typeof(A4)'),
            shouldBe('B1', valueS('double')),
            r('A1', 'c("a","b","c","d")'),
            shouldBe('B1', valueS('character')),
            exec(done)
          ]);
        });

        xit('plots shit', (done) => {
          _do([
            r('A1','qplot(x=\'x\',y=\'y\',data=data.frame(c(1,2)))'),
            shouldBeImage('A1'),
            exec(done)
          ]);
        });
      });

      describe('excel', () => {
        it('should evaluate sums', (done) => {
          _do([
            python('A1', 'range(10)'),
            excel('B1', '=A1+A2'),
            shouldBe('B1', valueI(1)),
            exec(done)
          ]);
        });

        it('should evaluate a literal', (done) => {
          _do([
            excel('A1', '1'),
            shouldBe('A1', valueI(1)),
            exec(done)
          ]);
        });

        it('should evaluate a string literal verbatim', (done) => {
          _do([
            excel('A1', '"hello"hello"hello"'),
            shouldBe('A1', valueS("\"hello\"hello\"hello\"")),
            exec(done)
          ]);
        });

        it('should include quotes in string literal with quotes', (done) => {
          _do([
            excel('A1', '"hello"'),
            shouldBe('A1', valueS("\"hello\"")),
            exec(done)
          ]);
        });

        it('should evaluate entire expression', (done) => {
          _do([
            excel('A1', '=SUM(1,2)ASDF"sadf'), //and not just match =SUM(1,2) and equal 3
            shouldBeError('A1'),
            exec(done)
          ]);
        });

        it('should evaluate nested formulas', (done) => {
          _do([
            excel('A1', '=SUM(1,SUM(2,3))'),
            shouldBe('A1', valueI(6)),
            exec(done)
          ]);
        });

        it('should recognize functions no matter how they are capitalized', (done) => {
          _do([
            excel('A1', '1'),
            excel('A2', '2'),
            excel('A3', '=sUm(A1,A2)'),
            shouldBe('A3', valueI(3)),
            exec(done)
          ]);
        });

        it('should recognize true and false no matter how they are capitalized', (done) => {
          _do([
            excel('A1', 'TrUe'),
            excel('A2', 'false'),
            shouldBe('A1', valueB(true)),
            shouldBe('A2', valueB(false)),
            exec(done)
          ]);
        });

        it('should recognize emptry strings', (done) => {
          _do([
            excel('A1', '   '),
            shouldBe('A1', valueS('   ')),
            exec(done)
          ]);
        });

        it('recognizes - and + prefix operators', (done) => { 
          _do([
            excel('A1', '=++--+-2'), 
            shouldBe('A1', valueI(-2)),
            exec(done)
          ]);
        });

        describe('abs', () => {
          it('should evaluate', (done) => {
            _do([
              python('A1', 'range(10)'),
              excel('B1', '=abs(A2)'),
              shouldBe('B1', valueI(1)),
              exec(done)
            ]);
          });

          it('should scalarize', (done) => {
            _do([
              python('A1', 'range(10)'),
              excel('B1', '=abs(A$1:A$10)'),
              copy('B1', 'B2:B10'),
              shouldBeL(
                _.range(10).map((i) => `B${i + 1}`),
                _.range(10).map(valueI)
              ),
              exec(done)
            ]);
          });
        });

        describe('equals', () => {
          it('should eval 1=1', (done) => {
            _do([
              python('A1', '1'),
              excel('B1', '=A1=1'),
              shouldBe('B1', valueB(true)),
              exec(done)
            ]);
          });

          it('should array-ize', (done) => {
            _do([
              python('A1', 'range(2)'),
              excel('B1', '{=A1:A2=1}'),
              shouldBeL(['B1', 'B2'], [false, true].map(valueB)),
              exec(done)
            ]);
          });
        });

        describe('exponentiation', () => {
          it('should evaluate integer exponentiation', (done) => {
            _do([
              python('A1', '10'),
              excel('B1', '=A1^2'),
              shouldBe('B1', valueI(100)),
              exec(done)
            ]);
          });

          it('should not raise 0 to the 0', (done) => {
            _do([
              python('A1', '0'),
              excel('B1', '=A1^0'),
              shouldBeError('B1'),
              exec(done)
            ]);
          });

          it('should allow negative numbers to be exponentiated with integers', (done) => {
            _do([
              excel('A1', '=(-2)^(-2)'),
              shouldBe('A1', valueD(0.25)), 
              exec(done)
            ]);
          });

          it('should not allow negative numbers to be exponentiated with floats', (done) => {
            _do([
              excel('A1', '=(-2)^(-2.0)'),
              shouldBeError('A1'),
              exec(done)
            ]);
          });
        });
      });

      describe('ocaml', () => {

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

        it('should cut properly', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            python('A2', '3'),
            python('B2', '4'),
            cut('A1:B2', 'B1:C2'),
            shouldBeNothing('A1'),
            shouldBeNothing('A2'),
            shouldBe('B1', valueI(2)),
            shouldBe('C1', valueI(3)),
            shouldBe('B2', valueI(3)),
            shouldBe('C2', valueI(4)),
            exec(done)
          ]);
        });

        it('should cut properly with blank cells', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            python('A2', '3'),
            cut('A1:B2', 'B1:C2'),
            shouldBeNothing('A1'),
            shouldBeNothing('A2'),
            shouldBe('B1', valueI(2)),
            shouldBe('C1', valueI(3)),
            shouldBe('B2', valueI(3)),
            exec(done)
          ]);
        });

        // works on backend but crashes the test
        xit('should not re-eval a non-head list cell with its expression unchanged', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('A2', 'range(10)'),
            shouldBe('A2', valueI(1)),
            exec(done)
          ]);
        });

        xit('should shrink a range based on a dependency', (done) => {
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
      describe('copy/cut/paste', () => {
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

        it('should re-evaluate copy/paste selections composed of only list heads', (done) => {
          _do([
            python('A1', '"John Smith"'),
            python('A2', '"Alex Zhu"'),
            python('A3', '"Bob Ghandi"'),
            python('B1', '[A1.split()]'),
            copy('B1', 'B2:B3'),
            shouldBe('C2', valueS("Zhu")),
            shouldBe('C3', valueS("Ghandi")),
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
                  locToExcel({ tl: {col: col + 3, row: row + 1},
                               br: {col: col + 3, row: row + 1}}),
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

        it('should refuse to copy to create a circular dependency', (done) => {
          _do([
            python('D1', '5'),
            python('C1', 'D1'),
            python('B1', 'A1 + 1'),
            shouldError(
              copy('B1', 'D1')
            ),
            shouldBe('D1', valueI(5)),
            exec(done)
          ]);
        });

        it('should refuse to copy out of bounds', (done) => {
          _do([
            python('A2', 'A1+1'),
            copy('A2', 'A1'),
            shouldBeError('A1'),
            exec(done)
          ]);
        });

        it('should copy expressions with both a list and a dependency to the list', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('B1', 'sum(A1:A10)'),
            copy('A1:B10', 'C1:D10'),
            shouldBe('D1', valueI(45)),
            exec(done)
          ]);
        });

        it('should successfully copy out of bounds expressions', (done) => {
          _do([
            python('A2', 'A1+1'),
            copy('A2', 'A1'),
            copy('A1', 'B1'),
            shouldBeError('B1'),
            exec(done)
          ]);
        });

        it('should not shift quoted excel references', (done) => {
          _do([
            python('B1', '"there"'),
            python('A2', '"A1"+A1'),
            copy('A2','B2'),
            shouldBe('B2', valueS('A1there')),

            python('A2', 'A1+"A1"'),
            copy('A2','B2'),
            shouldBe('B2', valueS('thereA1')),

            exec(done)
          ]);
        });

        it('should not shift quoted excel references with escaped chars', (done) => {
          _do([
            python('B1', '"there"'),

            python('A2', "A1+'A1'+A1+\"A1\"+\"\\\"A1\\\"\""),
            copy('A2','B2'),
            shouldBe('B2', valueS("thereA1thereA1\"A1\"")),
            exec(done)
          ]);
        });

        it('should not shift excel literals', (done) => {
          _do([
            excel('A1', 'A1"A1"'),
            copy('A1','B1'),
            shouldBe('B1', valueS('A1"A1"')),
            exec(done)
          ]);
        });


        it('should successfully copy and paste cells that depend on each other', (done) => {
          _do([
            python('A1', '1'),
            python('A2', 'A1 + 1'),
            python('B1', 'A2 + 1'),
            python('B2', 'A1 + A2 + B1'),
            copy('A1:B2', 'C1:D2'),
            shouldBeL(
              ['C1', 'C2', 'D1', 'D2'],
              [1, 2, 3, 6].map(valueI)
            ),
            exec(done)
          ]);
        });

        it('should copy an entire list without decoupling it', (done) => {
          _do([
            python('A1', 'range(10)'),
            copy('A1:A10', 'B1:B10'),
            expressionShouldBe('B1', 'range(10)'),
            exec(done)
          ]);
        });

        it('should decouple a partial list while copying it', (done) => {
          _do([
            python('A1', 'range(10)'),
            copy('A1:A2', 'B1:B2'),
            expressionShouldBe('B1', '0'),
            exec(done)
          ]);
        });

        it('should copy blank cells', (done) => {
          _do([
            python('A1', '1'),
            python('B2', '2'),
            copy('A1:A2', 'B1:B2'),
            shouldBeNothing('B2'),
            exec(done)
          ]);
        });

        it('should cut properly', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            python('A2', '3'),
            python('B2', '4'),
            cut('A1:B2', 'B1:C2'),
            shouldBeNothing('A1'),
            shouldBeNothing('A2'),
            shouldBe('B1', valueI(2)),
            shouldBe('C1', valueI(3)),
            shouldBe('B2', valueI(3)),
            shouldBe('C2', valueI(4)),
            exec(done)
          ]);
        });

        it('should cut properly with blank cells', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            python('A2', '3'),
            python('C2', '5'),
            cut('A1:B2', 'B1:C2'),
            shouldBeNothing('A1'),
            shouldBeNothing('A2'),
            shouldBe('B1', valueI(2)),
            shouldBe('C1', valueI(3)),
            shouldBe('B2', valueI(3)),
            shouldBeNothing('C2'),
            exec(done)
          ]);
        });
      });

      describe('repeat', () => {
        it('should repeat eval on Ctrl+Y', (done) => {
          _do([
            python('A1', '1'),
            repeat('A2:A10', 'A2'),
            shouldBe('A5', valueI(1)),
            exec(done)
          ]);
        });

        it('should repeat copy on Ctrl+Y', (done) => {
          _do([
            python('A2', 'A1+1'),
            python('B1', '1'),
            copy('A2', 'B2'),
            repeat('B3:B10', 'B3'),
            shouldBe('B5', valueI(5)),
            exec(done)
          ]);
        });

        it('should repeat delete on Ctrl+Y', (done) => {
          _do([
            python('A1', 'range(10)'),
            python('B1', '1'),
            delete_('B1'),
            repeat('A1:B10', 'B1'),
            shouldBeNothing('A1'),
            exec(done)
          ]);
        });

        it('should redo on Ctrl+Y after undo', (done) => {
          _do([
            python('A1', 'range(10)'),
            undo(),
            repeat('A69:A69', 'A69'),
            shouldBe('A5', valueI(4)),
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

        it('should undo a dependency cleanly', (done) => {
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

        it('should undo a copy', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            copy('A1:B1', 'C1:D1'),
            undo(),
            _forM_(['C1', 'D1'],
              shouldBeNothing
            ),
            exec(done)
          ]);
        });

        it('should undo a cut', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('B1', 'A1 + 1'),
            cut('A1:B1', 'C1:D1'),
            undo(),
            _forM_(['C1', 'D1'],
              shouldBeNothing
            ),
            shouldBe('A1', valueI(2)),
            shouldBe('B1', valueI(3)),
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

        it('should undo and redo copy and paste', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('A2', 'A1 + 1'),
            copy('A1:A2', 'B1:B2'),
            undo(),
            redo(),
            shouldBeL(
              ['B1', 'B2'],
              [2, 3].map(valueI)
            ),
            exec(done)
          ]);
        });

        it('should undo and redo cut and paste', (done) => {
          _do([
            python('A1', '1 + 1'),
            python('A2', 'A1 + 1'),
            cut('A1:A2', 'B1:B2'),
            undo(),
            redo(),
            shouldBeL(
              ['B1', 'B2'],
              [2, 3].map(valueI)
            ),
            shouldBeNothing('A1'),
            shouldBeNothing('A2'),
            exec(done)
          ]);
        });
      });

      describe('list cell behavior on copy/paste', () => {
        it('undoing should delete list key and not cause crashes', (done) => {
          // based off a crash that actually happened
          _do([
            python('A1', 'range(10)'),
            undo(),
            python('A1', 'range(10)'),
            undo(),
            redo(), 
            copy('A1:A10', 'B1:B10'),
            expressionShouldBe('B1', "range(10)"),
            exec(done)
          ]);
        });
      });
    });
  });

});
