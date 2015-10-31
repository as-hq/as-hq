import _ from 'lodash';

import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';
import TC from '../AS/TypeConversions';
import Store from '../stores/ASEvaluationStore';

import {promise, exec, fromToInclusive, _do, _doDefer} from './exec-monad';

let expect;
export function __injectExpect(exp) {
  expect = exp;
}

export function actionAPIResponse(actionPrmFn, responseFn) {
  return _doDefer([
    apiExec(actionPrmFn),
    exec(responseFn)
  ]);
}

export function indFromExcel(exLoc) {
  return Util.excelToIndex(exLoc);
}

export function rangeFromExcel(exLoc) {
  return Util.excelToRange(exLoc);
}

export function locToExcel(loc) {
  return Util.rangeToExcel(loc);
}

export function asIndex(loc) {
  return TC.simpleToASIndex(Util.excelToIndex(loc));
}

export function asRange(loc) {
  return TC.simpleToASRange(Util.excelToRange(loc));
}

export function apiExec(fn) {
  return promise((fulfill, reject) => {
    API.test(fn, {
      fulfill: fulfill,
      reject: reject
    });
  });
}

export function apiSyncExec(fn) {
  return promise((fulfill, reject) => {
    API.testSync(fn, {
      fulfill: fulfill,
      reject: reject
    });
  });
}

export function directAPIExec(fn) {
  return new Promise((fulfill, reject) => {
    API.test(fn, {
      fulfill: fulfill,
      reject: reject
    });
  });
}

export function openSheet() {
  return apiSyncExec(() => {
    API.openSheet();
  });
}

export function syncWindow() {
  return apiExec(() => {
    let range = { tl: {col: 0, row: 0}, br: {col: 100, row: 100 }},
      vWindow = TC.rangeToASWindow(range);
    API.updateViewingWindow(vWindow);
  });
}

export function clear() {
  return apiExec(() => {
    API.clear();
  });
}

export function init() {
  return apiExec(() => {
    API.initialize();
  });
}

export function cell(loc, xp, lang) {
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

export function python(loc, xp) {
  return cell(loc, xp, 'py');
}

export function r(loc, xp) {
  return cell(loc, xp, 'R');
}

export function excel(loc, xp) {
  return cell(loc, xp, 'excel');
}

export function ocaml(loc, xp) {
  return cell(loc, xp, 'ml');
}

export function copy(rng1, rng2) {
  return apiExec(() => {
    let [asRng1, asRng2] = [rng1, rng2].map(asRange);
    API.copy(asRng1, asRng2);
  });
}

export function repeat(rng, origin) {
  return apiExec(() => {
    let sel = {origin: indFromExcel(origin), range: rangeFromExcel(rng)}
    API.repeat(sel);
  });
}

export function cut(rng1, rng2) {
  return apiExec(() => {
    let [asRng1, asRng2] = [rng1, rng2].map(asRange);
    API.cut(asRng1, asRng2);
  });
}

export function undo() {
  return apiExec(() => {
    API.undo();
  });
}

export function redo() {
  return apiExec(() => {
    API.redo();
  });
}

export function delete_(rng) {
  return apiExec(() => {
    API.deleteRange(TC.simpleToASRange(rangeFromExcel(rng)));
  });
}

export function valueD(val) {
  return { tag: 'ValueD', contents: val };
}

export function valueI(val) {
  return { tag: 'ValueI', contents: val };
}

export function valueB(val) {
  return { tag: 'ValueB', contents: val };
}

export function valueS(val) {
  return { tag: 'ValueS', contents: val };
}

export function equalValues(val1, val2) {
  return _.isEqual(val1, val2);
}

// (() -> Promise a) -> (a -> Bool) -> (() -> Promise ())
export function responseShouldSatisfy(prf, fn) {
  return promise((fulfill, reject) => {
    prf().then((result) => {
      expect(fn(result)).toBe(true);
      fulfill();
    }).catch((error) => {
      reject(error);
    });
  });
}

export function shouldError(prf) {
  return responseShouldSatisfy(prf, ({ result: { tag } }) => tag === 'Failure');
}

export function messageShouldSatisfy(loc, fn) {
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

export function expressionShouldSatisfy(loc, fn) {
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

export function expressionShouldBe(loc, xp) {
  return expressionShouldSatisfy(loc, ({ expression }) => expression === xp);
}

export function valueShouldSatisfy(loc, fn) {
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
export function shouldBe(loc, val) {
  return valueShouldSatisfy(loc, (cv) => equalValues(cv, val));
}

export function shouldBeError(loc) {
  return valueShouldSatisfy(loc, ({ tag }) => (tag === 'ValueError' || tag == 'ValueExcelError'));
}

export function shouldBeImage(loc) {
  return valueShouldSatisfy(loc, ({ tag }) => (tag === 'ValueImage'));
}

export function shouldBeNothing(loc) {
  return messageShouldSatisfy(loc, (cs) => {
    console.log(`${loc} should be nothing`);
    //server should return either nothing at the location or a blank cell
    let isEmpty = (cs.length == 0) || (cs[0].cellExpression.expression == "");
    expect(isEmpty).toBe(true);
  });
}

// [String] -> [ASValue] -> (() -> Promise ())
export function shouldBeL(locs, vals) {
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

