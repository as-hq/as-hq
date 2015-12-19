/* @flow */

export type ASCartesianChartType = 'Line' | 'Bar' | 'Radar';

export type ASPolarChartType = 'PolarArea' | 'Pie' | 'Doughnut';

export type ASChartType =
  ASCartesianChartType | ASPolarChartType;

export type ASChartOptions = 'poop';// TODO

export type ASChartContext = {
  chartType: ASChartType;
  values: ?(Array<Array<number|string>> | Array<number|string>);
  xLabels: ?Array<string>;
  plotLabels: ?Array<string>;
  options: ASChartOptions;
};

export type XYDataset = {
  label: string;
  fillColor: string;
  strokeColor: string;
  pointColor: string;
  pointStrokeColor: string;
  pointHighlightFill: string;
  pointHighlightStroke: string;
  data: Array<number>;
};

export type BarDataset = {
  label: string;
  fillColor: string;
  strokeColor: string;
  highlightFill: string;
  highlightStroke: string;
  data: Array<number>;
};

export type CartesianDataset = XYDataset | BarDataset;

export type PolarDataset = {
  value: number;
  color: string;
  highlight: string;
  label: string;
};

export type ASCartesianData = {
  labels: Array<string>;
  datasets: Array<CartesianDataset>;
};

export type ASPolarData = Array<PolarDataset>;

export type ASChartData = ASCartesianData | ASPolarData;
