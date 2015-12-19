/* @flow */

import type {
  ASCell
} from '../../types/Eval';

import type {
  ASChartType,
  ASCartesianChartType,
  ASPolarChartType
} from './types';

import Constants from '../../Constants';

let {ChartTypes} = Constants;

export default {
  cellToChartVal(c: ?ASCell): number {
    if (c !== null && c !== undefined) {
      switch (c.cellValue.tag) {
        case "ValueI":
        case "ValueD":
          return c.cellValue.contents;
        default:
          return 0;
      };
    } else return 0;
  },

  cellToLabel(c: ?ASCell): string {
    if (c !== null && c !== undefined) {
      switch (c.cellValue.tag) {
        case "ValueS":
          return c.cellValue.contents;
        default:
          return "DefaultLabel";
      };
    } else return "DefaultLabel";
  },

  isCartesian(chartType: ASChartType): boolean {
    return ["Line", "Bar", "Radar"].includes(chartType);
  },

  isPolar(chartType: ASChartType): boolean {
    return ["PolarArea", "Pie", "Doughnut"].includes(chartType);
  },

  // a -> Int -> [a]
  repeat<T>(val: T, n: number): Array<T> {
    return Array(...Array(n)).map(() => val);
  },

  // Int -> [Nat]
  takeNat<T>(n: number): Array<number> {
    return Array.from(new Array(n), (x,i) => i+1)
  },

  // [[a]] -> ThrowsError [a]
  reduceNestedArrayStr(arr: Array<Array<string>>): Array<string> {
    console.assert(arr.constructor === Array && arr.every((elem) => { return elem.constructor === Array; }));
    console.assert(arr.length == 1 || arr.every((subArr) => { return subArr.length == 1; }));
    if (arr.length == 1) return arr[0];
    else return arr.map((elem) => { return elem[0]; });
  },

  // wtf flow... can't make this a polymorphic function
  reduceNestedArrayNum(arr: Array<Array<number>>): Array<number> {
    console.assert(arr.constructor === Array && arr.every((elem) => { return elem.constructor === Array; }));
    console.assert(arr.length == 1 || arr.every((subArr) => { return subArr.length == 1; }));
    if (arr.length == 1) return arr[0];
    else return arr.map((elem) => { return elem[0]; });
  }
}
