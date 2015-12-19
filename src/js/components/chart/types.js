/* @flow */

export type ASCartesianChartType = 'Line' | 'Bar' | 'Radar';

export type ASPolarChartType = 'PolarArea' | 'Pie' | 'Doughnut';

export type ASChartType =
  ASCartesianChartType | ASPolarChartType;

export type ASChartContext = {
  chartType: ASChartType;
  values: ?(Array<Array<number|string>> | Array<number|string>);
  xLabels: ?Array<string>;
  plotLabels: ?Array<string>;
};
