import _ from 'underscore';
import fs from 'fs';
import jasminePit from 'jasmine-pit';

jasminePit.install(window);

function empty() {
  return new Promise((fulfill, reject) => { fulfill(); });
}

function cellFile(testId, excFormatPos) {
  //TODO
}

function clear() {
  return () => {
    return new Promise((fulfill, reject) => {
      fulfill(); //TODO
    });
  };
}

function sheet(name) {
  return () => {
    return new Promise((fulfill, reject) => {
      fulfill(); //TODO
    });
  };
}

function cell(loc, xp, lang) {
  return () => {
    return new Promise((fulfill, reject) => {
      fulfill(); //TODO
    });
  };
}

function valueD(val) {
  //TODO
}

function equalValues(val1, val2) {
  //TODO
}

// Loc -> Val -> (() -> Promise ())
function shouldBe(loc, val) {
  return () => {
    return new Promise((fulfill, reject) => {
      //TODO
      fulfill();
    });
  }
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

describe('backend', () => {
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
          clear()
        ]).then(done);
      });

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
