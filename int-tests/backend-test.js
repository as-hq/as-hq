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

  function cellFile(filename) {
    //TODO
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
        { range: Util.excelToLoc(loc) },
        { exp: xp, lang: { Server: langMap[lang] } }
      );
      let msg = Converter.createEvalRequestFromASCell(cell);
      API.send(msg);
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

  // String -> ASValue -> (() -> Promise ())
  function shouldBe(loc, val) {
    return promise((fulfill, reject) => {
      console.log(`${loc} should be ${JSON.stringify(val)}`);

      API.test(() => {
        API.sendGetRequest([ Util.excelToLoc(loc) ]);
      }, {
        fulfill: (result) => {
          let [{ cellValue }] = Converter.clientCellsFromServerMessage(result);
          expect(equalValues(cellValue, val)).toBe(true);

          fulfill();
        },
        reject: reject
      });
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

  function cellsOf(testId) {
    let files = fs.readdirSync('./eval-tests');
    let acc = [];

    let grp = 0;
    while (true) {
      let grpName = `${testId}$${grp}`;
      let relevantFiles = files.filter((fname) => fname.indexOf(grpName) === 0 );

      if (relevantFiles.length === 0) break;

      relevantFiles.forEach((fname) => {
        let [__, cellLoc, cellLang] = fname.match(/^[^\$]*\$[^\$]*\$([^\.]*)\.([^\$]*)$/);
        let cellXp = fs.readFileSync(`./eval-tests/${fname}`, 'utf8');

        acc = acc.concat(cell(cellLoc, cellXp, cellLang));
      });

      grp++;
    }

    return _doDefer(acc);
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
    it('clears a sheet', () => {
    });

    it('creates a new sheet', () => {
    });

    it('creates a new workbook', () => {
    });

    it('gets cells', () => {
    });
  });

  describe('eval', () => {
    describe('initial eval', () => {
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
          //TODO: clear(),
          logP('Finished preparing.'),
          exec(done)
        ]);
      });

      it('should evaluate at all', (done) => {
        _do([
          cell('A1', '1 + 1', 'py'),
          shouldBe('A1', valueI(2)),
          exec(done)
        ]);
      });

      xit('should evaluate a Python cell', (done) => {
        _do([
          cellsOf('py1'),
          shouldBe('A1', valueI(1)),
          exec(done)
        ]);
      });

      xit('should evaluate two Python cells, dependent', (done) => {
        _do([
          cell('A1', '1 + 1', 'py'),
          cell('A2', 'A1 + 1', 'py'),
          shouldBe('A1', valueI(2)),
          shouldBe('A2', valueI(3)),
          exec(done)
        ]);
      });

      xit('should evaluate a range and expand it', (done) => {
        _do([
          cell('A1', 'range(10)', 'py'),
          _forM_(_.range(10), (i) => {
            return shouldBe(`A${i}`, valueI(i));
          }),
          exec(done)
        ]);
      });
    });

    describe('repl eval', () => {
    });

    describe('update propagation', () => {
    });

    describe('dag updates', () => {
    });
  });

  describe('cell transforms', () => {
    describe('cut/paste', () => {
    });

    describe('copy/paste', () => {
    });
  });

  describe('vcs', () => {
    describe('undo', () => {
    });

    describe('redo', () => {

    });
  });
});
