jest.autoMockOff();

import _ from 'underscore';
import fs from 'fs';
import jasminePit from 'jasmine-pit';

jasminePit.install(window);

describe('backend', () => {
  const API = require('../src/js/actions/ASApiActionCreators');
  const Converter = require('../src/js/AS/Converter');
  const Util = require('../src/js/AS/Util');

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

  function apiExec(fn) {
    return promise((fulfill, reject) => {
      API.test(fn, {
        fulfill: fulfill,
        reject: reject
      });
    });
  }

  function sheet(name) {
    /*
    return apiExec(() => {

    });
    */
    return () => {
      return new Promise((fulfill, reject) => {
        fulfill(); //TODO
      });
    };
  }

  // monadic log operation, String -> (() -> Promise ())
  function logP(str) {
    return promise((fulfill, reject) => {
      console.log(str);
      fulfill();
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
    //TODO
  }

  function equalValues(val1, val2) {
    //TODO
  }

  // String -> ASValue -> (() -> Promise ())
  function shouldBe(loc, val) {
    return promise((fulfill, reject) => {
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
  function _do(promises) {
    return promises.reduce((acc, cur) => {
      return acc.then(cur);
    }, empty());
  }

  // [() -> Promise a] -> (() -> Promise ())
  function _doDefer(promises) {
    return () => {
      return _do(promises);
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
      beforeEach((done) => {
        _do([
          logP('Initializing...'),
          init(),
          logP('Clearing sheet...'),
          clear()
        ]).then(done);
      });

      //pit: a jest-compatible promise version of "it()"
      pit('should evaluate at all', () => {
        return _do([
          cell('A1', '1 + 1', 'py'),
          shouldBe('A1', valueD(2))
        ]);
      });

      pit('should evaluate a Python cell', () => {
        return _do([
          cellsOf('py1'),
          shouldBe('A1', valueD(1))
        ]);
      });

      pit('should evaluate two Python cells, dependent', () => {
        return _do([
          cell('A1', '1 + 1', 'py'),
          cell('A2', 'A1 + 1', 'py'),
          shouldBe('A1', valueD(2)),
          shouldBe('A2', valueD(3))
        ]);
      });

      pit('should evaluate a range and expand it', () => {
        return _do([
          cell('A1', 'range(10)', 'py'),
          _forM_(_.range(10), (i) => {
            return shouldBe(`A${i}`, valueD(i));
          })
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
