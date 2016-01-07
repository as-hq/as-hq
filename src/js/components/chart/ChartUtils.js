/* @flow */

import type {
  NakedRange
} from '../../types/Eval';

import type {
  ASChartType,
  ASCartesianChartType,
  ASPolarChartType,
  RGBColor
} from './types';

import ASCell from '../../classes/ASCell';

import Constants from '../../Constants';

let {ChartTypes} = Constants;
let colorIndex = 0;
const colors = [
  {r:216, g:27, b:96 },
  {r:0, g:137, b:123},
  {r:94, g:53, b:177},
  {r:97, g:97, b:97},
  {r:229, g:57, b:53},
  {r:142, g:36, b:170},
  {r:67, g:160, b:71},
  {r:84, g:110, b:122},
  {r:57, g:73, b:171},
  {r:244, g:81, b:30}
];

const ChartUtils = {
  cellToChartVal(c: ?ASCell): number {
    if (c !== null && c !== undefined) {
      switch (c.value.tag) {
        case "ValueI":
        case "ValueD":
          return c.value.contents;
        default:
          return 0;
      };
    } else return 0;
  },

  cellToLabel(c: ?ASCell): string {
    if (c !== null && c !== undefined) {
      switch (c.value.tag) {
        case "ValueI":
        case "ValueD":
        case "ValueS":
          return c.value.contents.toString();
        default:
          return "DefaultLabel";
      };
    } else return "DefaultLabel";
  },

  generatePlotLabels(n: number): Array<string> {
    return ChartUtils.takeNat(n).map((i) => `Plot${i}`)
  },

  generateXLabels(n: number): Array<string> {
    return ChartUtils.takeNat(n).map((i) => i.toString());
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
    return Array.from(new Array(n), (x,i) => i+1);
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

    if (ChartUtils.isVector(arr)) {
      if (arr.length == 1) return arr[0];
      else return arr.map((elem) => { return elem[0]; });
    } else {
      // console.error("Array passed in to reduceNestedArray was not even one-dimensional");
      return null;
    }
  },

  generateRGB(): RGBColor {
    colorIndex = (colorIndex + 1) % colors.length;
    return colors[colorIndex];
  }
}

export default ChartUtils;
