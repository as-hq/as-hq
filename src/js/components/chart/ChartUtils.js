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
    // ASCell -> Maybe JSVal
  cellToJSVal(c: ASCell): ?(string|number) {
    switch (c.cellValue.tag) {
      case "ValueI":
      case "ValueD":
      case "ValueS":
        return c.cellValue.contents;
      default:
        return null;
    };
  },

  isCartesian(chartType: ASChartType): boolean {
    return [ChartTypes.Line, ChartTypes.Bar, ChartTypes.Radar].includes(chartType);
  },

  isPolar(chartType: ASChartType): boolean {
    return [ChartTypes.PolarArea, ChartTypes.Pie, ChartTypes.Doughnut].includes(chartType);
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
  reduceNestedArray<T>(arr: Array<Array<T>>): Array<T> {
    console.assert(arr.constructor === Array && arr.every((elem) => { return elem.constructor === Array; }));
    console.assert(arr.length == 1 || arr.every((subArr) => { return subArr.length == 1; }));
    if (arr.length == 1) return arr[0];
    else return arr.map((elem) => { return elem[0]; });
  }
}
