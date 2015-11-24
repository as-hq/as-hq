/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ASRange,
  ValueD,
  ValueI,
  ValueS,
  ValueB,
  NoValue,
  ASValue,
  ASExpression,
  ASLanguage,
  ASCellProp,
  ASCell, 
  VAlignType, 
  HAlignType
} from '../types/Eval';

import type {
  ASServerMessage
} from '../types/Messages';

import type {
  ASTestLanguage,
  Prf,
  Matcher
} from '../types/Tests';

import {
  logDebug
} from '../AS/Logger';

import _ from 'lodash';

import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';
import TC from '../AS/TypeConversions';
import Store from '../stores/ASEvaluationStore';
import Constants from '../Constants';

import {promise, exec, fromToInclusive, _do, _doDefer} from './exec-monad';
let {expect: exp} = require('./test-framework');

let expect: (val: any) => Matcher = exp;
export function __injectExpect(exp: (val: any) => Matcher) {
  expect = exp;
}

export function actionAPIResponse(
  actionPrmFn: () => void,
  responseFn: () => void
): Prf {
  return _doDefer([
    apiExec(actionPrmFn),
    exec(responseFn)
  ]);
}

export function indFromExcel(exLoc: string): NakedIndex {
  return Util.excelToIndex(exLoc);
}

export function rangeFromExcel(exLoc: string): NakedRange {
  return Util.excelToRange(exLoc);
}

export function locToExcel(loc: NakedRange): string {
  return Util.rangeToExcel(loc);
}

export function numToAlpha(num: number): string {
  return String.fromCharCode(num + 'A'.charCodeAt(0));
}

export function asIndex(loc: string): ASIndex {
  return TC.simpleToASIndex(Util.excelToIndex(loc));
}

export function asRange(loc: string): ASRange {
  return TC.simpleToASRange(Util.excelToRange(loc));
}

export function apiExec(fn: () => void): Prf {
  return promise((fulfill, reject) => {
    API.test(fn, {
      fulfill: fulfill,
      reject: reject
    });
  });
}

export function apiSyncExec(fn: () => void): Prf {
  return promise((fulfill, reject) => {
    API.testSync(fn, {
      fulfill: fulfill,
      reject: reject
    });
  });
}

export function directAPIExec(fn: () => void): Promise {
  return new Promise((fulfill, reject) => {
    API.test(fn, {
      fulfill: fulfill,
      reject: reject
    });
  });
}

export function openSheet(): Prf {
  return apiExec(() => {
    API.openSheet();
  });
}

export function syncWindow(): Prf {
  return apiExec(() => {
    let range = { tl: {col: 0, row: 0}, br: {col: 100, row: 100 }},
      vWindow = TC.rangeToASWindow(range);
    API.updateViewingWindow(vWindow);
  });
}

export function clear(): Prf {
  return apiExec(() => {
    API.clearSheet();
  });
}

export function init(): Prf {
  return apiExec(() => {
    API.initialize();
  });
}

export function cell(loc: string, xp: string, lang: ASTestLanguage): Prf {
  return apiExec(() => {
    let langMap = {
      'py': Constants.Languages.Python,
      'R': Constants.Languages.R,
      'excel': Constants.Languages.Excel,
      'ml': Constants.Languages.OCaml
    };
    let idx   = Util.excelToIndex(loc),
        xpObj = { expression: xp, language: langMap[lang] };
    API.evaluate(idx, xpObj);
  });
}

export function python(loc: string, xp: string): Prf {
  return cell(loc, xp, 'py');
}

export function r(loc: string, xp: string): Prf {
  return cell(loc, xp, 'R');
}

export function ocaml(loc: string, xp: string): Prf {
  return cell(loc, xp, 'ml');
}

export function excel(loc: string, xp: string): Prf {
  return cell(loc, xp, 'excel');
}

export function evalHeader(xp: string, lang: ASLanguage): Prf {
  return apiExec(() => {
    API.evaluateHeader(xp, lang);
  });
}

export function pythonEvalHeader(xp: string): Prf {
  return evalHeader(xp, 'Python');
}


export function repeat(rng: string, origin: string): Prf {
  return apiExec(() => {
    let sel = {origin: indFromExcel(origin), range: rangeFromExcel(rng)}
    API.repeat(sel);
  });
}

export function insertCol(c: number): Prf {
  return apiExec(() => {
    API.insertCol(c);
  });
}

export function insertRow(r: number): Prf {
  return apiExec(() => {
    API.insertRow(r);
  });
}

export function deleteCol(c: number): Prf {
  return apiExec(() => {
    API.deleteCol(c);
  });
}

export function deleteRow(r: number): Prf {
  return apiExec(() => {
    API.deleteRow(r);
  });
}

export function dragCol(c1: number, c2: number): Prf {
  return apiExec(() => {
    API.dragCol(c1, c2);
  });
}

export function dragRow(r1: number, r2: number): Prf {
  return apiExec(() => {
    API.dragRow(r1, r2);
  });
}

export function copy(rng1: string, rng2: string): Prf {
  return apiExec(() => {
    let [asRng1, asRng2] = [rng1, rng2].map(asRange);
    API.copy(asRng1, asRng2);
  });
}

export function cut(rng1: string, rng2: string): Prf {
  return apiExec(() => {
    let [asRng1, asRng2] = [rng1, rng2].map(asRange);
    API.cut(asRng1, asRng2);
  });
}

export function undo(): Prf {
  return apiExec(() => {
    API.undo();
  });
}

export function redo(): Prf {
  return apiExec(() => {
    API.redo();
  });
}

export function delete_(rng: string): Prf {
  return apiExec(() => {
    API.deleteRange(TC.simpleToASRange(rangeFromExcel(rng)));
  });
}

export function toggleProp(rng: string, prop: ASCellProp): Prf {
  return apiExec(() => {
    API.toggleProp(prop, rangeFromExcel(rng));
  });
}

export function setTextColor(rng: string, contents: string): Prf {
  return apiExec(() => {
    API.setTextColor(contents, rangeFromExcel(rng));
  });
}

export function setFillColor(rng: string, contents: string): Prf {
  return apiExec(() => {
    API.setFillColor(contents, rangeFromExcel(rng));
  });
}

export function setVAlign(rng: string, contents: VAlignType): Prf {
  return apiExec(() => {
    API.setVAlign(contents, rangeFromExcel(rng));
  });
}

export function setHAlign(rng: string, contents: HAlignType): Prf {
  return apiExec(() => {
    API.setHAlign(contents, rangeFromExcel(rng));
  });
}

export function setFontSize(rng: string, contents: number): Prf {
  return apiExec(() => {
    API.setFontSize(contents, rangeFromExcel(rng));
  });
}

export function setFontName(rng: string, contents: string): Prf {
  return apiExec(() => {
    API.setFontName(contents, rangeFromExcel(rng));
  });
}

export function setFormat(rng: string, formatType: string): Prf {
  return apiExec(() => {
    API.setFormat(formatType, rangeFromExcel(rng));
  });
}

export function setUrl(rng: string, urlLink: string): Prf {
  return apiExec(() => {
    API.setUrl(urlLink, rangeFromExcel(rng));
  });
}

export function valueD(val: number): ValueD {
  return { tag: 'ValueD', contents: val };
}

export function valueI(val: number): ValueI {
  return { tag: 'ValueI', contents: val };
}

export function valueB(val: boolean): ValueB {
  return { tag: 'ValueB', contents: val };
}

export function valueS(val: string): ValueS {
  return { tag: 'ValueS', contents: val };
}

export function noValue(): NoValue {
  return {tag: 'NoValue', contents: []};
}

export function valueNaN(): ValueNaN {
  return {tag: 'ValueNaN', contents: []};
}

export function valueInf(): ValueInf {
  return {tag: 'ValueInf', contents: []};
}

function isNumeric(n: any) {
  return !isNaN(parseFloat(n)) && isFinite(n);
}

export function equalValues(val1: ASValue, val2: ASValue): boolean {
  logDebug(`${JSON.stringify(val1)} should be equal to ${JSON.stringify(val2)}`);
  expect(val1).not.toBe(undefined);
  expect(val2).not.toBe(undefined);
  if (val1.tag === 'ValueD' && val2.tag === 'ValueD') {
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

export function equalValuesExact(val1: ASValue, val2: ASValue): boolean {
  logDebug(`${JSON.stringify(val1)} should be equal to ${JSON.stringify(val2)}`);
  expect(val1).not.toBe(undefined);
  expect(val2).not.toBe(undefined);
  return _.isEqual(val1, val2);
}

// (() -> Promise a) -> (a -> Bool) -> (() -> Promise ())
export function responseShouldSatisfy<A>(prf: Prf<A>, fn: (a: A) => boolean): Prf {
  return promise((fulfill, reject) => {
    prf().then((result) => {
      expect(fn(result)).toBe(true);
      fulfill();
    }).catch((error) => {
      reject(error);
    });
  });
}

export function shouldError(prf: Prf): Prf {
  return responseShouldSatisfy(prf, ({ result: { tag } }) => tag === 'Failure');
}

export function messageShouldSatisfy(loc: string, fn: (cs: Array<ASCell>) => void): Prf {
  return promise((fulfill, reject) => {
    API.test(() => {
      API.getIndices([ asIndex(loc) ]);
    }, {
      fulfill: (result: ?ASServerMessage) => {
        if (! result) {
          reject();
          return;
        }

        let {payload} = result;
        if (payload.tag !== 'PayloadCL') {
          reject();
          return;
        }

        let {contents: cs} = payload;
        fn(cs);

        fulfill();
      },
      reject: reject
    });
  });
}

export function expressionShouldSatisfy(loc: string, fn: (exp: ASExpression) => boolean): Prf {
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

export function expressionShouldBe(loc: string, xp: string): Prf {
  return expressionShouldSatisfy(loc, ({ expression }) => expression === xp);
}

export function shouldHaveProp(loc: string, prop: ASCellProp): Prf {
  return messageShouldSatisfy(loc, (cs) => {
    logDebug(`${loc} cell should have prop ${prop}`);

    expect(cs.length).not.toBe(0);
    if (cs.length == 0) {
      return;
    }

    let [{ cellProps }] = cs;
    expect(cellProps.map((c) => c.tag).indexOf(prop)).not.toBe(-1, "meaning the prop wasn't found");
  });
}

export function shouldNotHaveProp(loc: string, prop: ASCellProp): Prf {
  return messageShouldSatisfy(loc, (cs) => {
    logDebug(`${loc} cell should have prop ${prop}`);

    if (cs.length == 0) {
      expect(true).toBe(true);
      return;
    }

    let [{ cellProps }] = cs;
    expect(cellProps.map((c) => c.tag).indexOf(prop)).toBe(-1, "meaning the prop wasn't found");
  });
}

export function valueShouldSatisfy(loc: string, fn: (val: ASValue) => boolean): Prf {
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
export function shouldBe(loc: string, val: ASValue): Prf {
  return valueShouldSatisfy(loc, (cv) => equalValues(cv, val));
}

export function shouldBeExact(loc: string, val: ASValue): Prf {
  return valueShouldSatisfy(loc, (cv) => equalValuesExact(cv, val));
}

export function shouldBeError(loc: string): Prf {
  return valueShouldSatisfy(loc, ({ tag }) => (tag === 'ValueError'));
}

export function shouldBeImage(loc: string): Prf {
  return valueShouldSatisfy(loc, ({ tag }) => (tag === 'ValueImage'));
}

export function shouldBeNothing(loc: string): Prf {
  return messageShouldSatisfy(loc, (cs) => {
    logDebug(`${loc} should be nothing`);
    //server should return either nothing at the location or a blank cell
    let isEmpty = (cs.length == 0) || (cs[0].cellExpression.expression == "");
    expect(isEmpty).toBe(true);
  });
}

export function shouldBeSerialized(loc: string): Prf {
  return valueShouldSatisfy(loc, ({ tag }) => (tag === 'ValueSerialized'));
}

export function shouldBeDecoupled(loc: string): Prf {
  return expressionShouldSatisfy(loc, (xp) => (!xp.hasOwnProperty('rangeKey')));
}

export function shouldBeCoupled(loc: string): Prf {
  return !shouldBeDecoupled(loc);
}

// [String] -> [ASValue] -> (() -> Promise ())
export function shouldBeL(locs: Array<string>, vals: Array<ASValue>): Prf {
  return promise((fulfill, reject) => {
    API.test(() => {
      API.getIndices(locs.map(asIndex));
    }, {
      fulfill: (result: ?ASServerMessage) => {
        if (! result || result.payload.tag !== 'PayloadCL') {
          reject();
          return;
        }

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
