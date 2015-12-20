/* @flow */

import type {
  ASCell,
  NakedRange
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

  generatePlotLabels(n: number): Array<string> {
    return this.takeNat(n).map((i) => `Plot${i}`)
  },

  generateXLabels(n: number): Array<string> {
    return this.takeNat(n).map((i) => i.toString());
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

  isVector<T>(arr:Array<Array<T>>): boolean {
    let isVVector = arr.length == 1,
        isHVector = arr.every((elem) => elem.length == 1);
    return isVVector || isHVector;
  },

  isVectorReference(rng: NakedRange): boolean {
    let {tl, br} = rng;
    return (tl.row == br.row) || (tl.col == br.col);
  },

  reduceNestedArrayStr(arr: Array<Array<string>>): ?Array<string> {
    let isVVector = arr.length == 1,
        isHVector = arr.every((elem) => elem.length == 1);
    if (isVVector || isHVector) {
      if (arr.length == 1) return arr[0];
      else return arr.map((elem) => { return elem[0]; });
    } else {
      // console.error("Array passed in to reduceNestedArray was not even one-dimensional");
      return null;
    }
  },

  // wtf flow... can't make this a polymorphic function
  reduceNestedArrayNum(arr: Array<Array<number>>): ?Array<number> {

    if (this.isVector(arr)) {
      if (arr.length == 1) return arr[0];
      else return arr.map((elem) => { return elem[0]; });
    } else {
      // console.error("Array passed in to reduceNestedArray was not even one-dimensional");
      return null;
    }
  }
}
