import {ChartTypes} from '../../Constants';

export default {
    // ASCell -> Maybe JSVal
  cellToJSVal(c) {
    switch (c.cellValue.tag) {
      case "ValueI":
      case "ValueD":
      case "ValueB":
      case "ValueS":
        return c.cellValue.contents;
      default:
        return null;
    };
  },

  isCartesian(chartType) { return [ChartTypes.Line, ChartTypes.Bar, ChartTypes.Radar].includes(chartType); },

  isPolar(chartType) { return [ChartTypes.PolarArea, ChartTypes.Pie, ChartTypes.Doughnut].includes(chartType); },

  // a -> Int -> [a]
  repeat(val, n) {
    return Array(...Array(n)).map(() => val);
  },

  // Int -> [Nat]
  takeNat(n) {
    return Array.from(new Array(n), (x,i) => i+1)
  },

  // [[a]] -> ThrowsError a
  reduceNestedArray(arr) {
    console.assert(arr.constructor === Array && arr.every((elem) => { return elem.constructor === Array; }));
    console.assert(arr.length == 1 || arr.every((subArr) => { return subArr.length == 1; }));
    if (arr.length == 1) return arr[0];
    else return arr.map((elem) => { return elem[0]; });
  },

}
