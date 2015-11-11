import {
  logDebug
} from '../AS/Logger';

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

export function numToAlpha(num) {
  return String.fromCharCode(num + 'A'.charCodeAt(0));
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
    API.clearSheet();
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

export function repeat(rng, origin) {
  return apiExec(() => {
    let sel = {origin: indFromExcel(origin), range: rangeFromExcel(rng)}
    API.repeat(sel);
  });
}

export function insertCol(c) {
  return apiExec(() => {
    API.insertCol(c);
  });
}

export function insertRow(r) {
  return apiExec(() => {
    API.insertRow(r);
  });
}

export function deleteCol(c) {
  return apiExec(() => {
    API.deleteCol(c);
  });
}

export function deleteRow(r) {
  return apiExec(() => {
    API.deleteRow(r);
  });
}

export function dragCol(c1, c2) {
  return apiExec(() => {
    API.dragCol(c1, c2);
  });
}

export function dragRow(r1, r2) {
  return apiExec(() => {
    API.dragRow(r1, r2);
  });
}

export function copy(rng1, rng2) {
  return apiExec(() => {
    let [asRng1, asRng2] = [rng1, rng2].map(asRange);
    API.copy(asRng1, asRng2);
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

export function toggleTag(rng, tag) {
  return apiExec(() => {
    API.toggleTag(tag, rangeFromExcel(rng));
  });
}

export function setTag(rng, tag, val) {
  return apiExec(() => {
    API.setTag(tag, val, rangeFromExcel(rng));
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

export function noValue() {
  return {tag: 'NoValue', contents: []};
}

function isNumeric(n) {
  return !isNaN(parseFloat(n)) && isFinite(n);
}

export function equalValues(val1, val2) {
  logDebug(`${JSON.stringify(val1)} should be equal to ${JSON.stringify(val2)}`);
  if (val1.tag == 'ValueD') {
    // eps = 10^-3
    var eps = 0.001;
    if (isNumeric(val1.contents) && isNumeric(val2.contents)) {
      return (val1.contents < val2.contents + eps
          && val2.contents - eps <  val1.contents);
    }
    return false;
  }
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
    logDebug(`${loc} expression should satisfy ${fn.toString()}`);

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

export function shouldHaveTag(loc, tag) {
  return messageShouldSatisfy(loc, (cs) => {
    logDebug(`${loc} cell should have tag ${tag}`);

    expect(cs.length).not.toBe(0);
    if (cs.length == 0) {
      return;
    }

    let [{ cellTags }] = cs;
    expect(cellTags.map((c) => c.tag).indexOf(tag)).not.toBe(-1, "meaning the tag wasn't found");
  });
}

export function shouldNotHaveTag(loc, tag) {
  return messageShouldSatisfy(loc, (cs) => {
    logDebug(`${loc} cell should have tag ${tag}`);

    expect(cs.length).not.toBe(0);
    if (cs.length == 0) {
      return;
    }

    let [{ cellTags }] = cs;
    expect(cellTags.map((c) => c.tag).indexOf(tag)).toBe(-1, "meaning the tag wasn't found");
  });
}

export function valueShouldSatisfy(loc, fn) {
  return messageShouldSatisfy(loc, (cs) => {
    logDebug(`${loc} should satisfy ${fn.toString()}`);

    expect(cs.length).not.toBe(0, 'which was the length of the cell value message');
    if (cs.length == 0) {
      return;
    }

    let [{ cellValue }] = cs;
    logDebug(`Cell value: ${JSON.stringify(cellValue)}`);
    expect(fn(cellValue)).toBe(true, `to satisfy ${fn.toString()} with ${cellValue}`);
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
    logDebug(`${loc} should be nothing`);
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

export function setUITestMode() {
  API.setUITestMode();
}

export function unsetUITestMode() {
  API.unsetUITestMode();
}
