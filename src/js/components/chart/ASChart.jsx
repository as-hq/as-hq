/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ASCell
} from '../../types/Eval';

import type {
  ASChartType,
  ASChartContext,
  ASChartData
} from './types';

import React from 'react';
import Chart from 'react-chartjs';

import Constants from '../../Constants';
import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';
import U from '../../AS/Util';
import CU from './ChartUtils';

let {Location: {isContainedInLocs}} = U;

let {ChartTypes} = Constants;

type ASChartProps = {
  chartContext: ASChartContext;
  valueRange: NakedRange;
  sheetId: string;
};

type ASChartState = {
  data: ?ASChartData;
  valueRange: NakedRange;
  sheetId: string;
  chartType: ASChartType;
};

export default class ASChart extends React.Component<{}, ASChartProps, ASChartState> {
  constructor(props: ASChartProps) {
    super(props);

    this.state = {
      data: null,
      valueRange: props.valueRange,
      sheetId: props.sheetId,
      chartType: 'Bar'
    };
  }

  componentDidMount() {
    CellStore.addChangeListener(this._onDataChange);
  }

  componentWillUnmount() {
    CellStore.removeChangeListener(this._onDataChange);
  }

  componentWillReceiveProps(newProps: ASChartProps) {
    // reconstruct chart with options here
    let chartData = this._contextToData(newProps.chartContext);
    let newState = {
      data: chartData,
      valueRange: newProps.valueRange,
      sheetId: newProps.sheetId,
      chartType: newProps.chartContext.chartType,
    };
    this.replaceState(newState);
  }

  // data binding
  _onDataChange() {
    let filteredCells = CellStore.getLastUpdatedCells().filter(this._isListening.bind(this));
    this._updateData(filteredCells);
  }

  _updateData(cs: Array<ASCell>) {
    let newData = this.state.data;
    cs.forEach((c) => {
      let {col, row} = this._getRelativeIndex(c.cellLocation.index);
      let val = CU.cellToJSVal(c.cellValue);
      newData.datasets[col].data[row] = val;
    });
    this.setState({data: newData});
  }

  // ChartContext -> ChartData
  _contextToData(ctx: ASChartContext): ASChartData {
    if (CU.isCartesian(ctx.chartType)) {
      return {
        labels: ctx.xLabels,
        datasets: this._generateCartesianDatasets(ctx)
      };
    } else if (CU.isPolar(ctx.chartType)) {
      return this._generatePolarDatasets(ctx);
    }
  }

  // #needsrefactor I want an either type here.
  // the next two functions are quite repetitive, but they do return different types...

  // ChartContext -> [CartesianDataset]
  _generateCartesianDatasets(ctx) {
    let datasets = Array(ctx.values.length);
    for (var i=0; i<ctx.values.length; i++) {
      datasets[i] = this._generateDatasetGraphicOptions(ctx.chartType)
        .assign({
          label: ctx.plotLabels[i],
          data: ctx.values[i]
        });
    } return datasets;
  }

  // ChartContext -> [PolarDataset]
  _generatePolarDatasets(ctx) {
    let datasets = Array(ctx.values.length);
    for (var i=0; i<ctx.values.length; i++) {
      datasets[i] = this._generateDatasetGraphicOptions(ctx.chartType)
        .assign({
          label: ctx.plotLabels[i],
          value: ctx.values[i]
        });
    } return datasets;
  }

  // ChartType -> GraphicOptions
  _generateDatasetGraphicOptions(chartType: ASChartType) {
    switch(chartType) {
      case ChartTypes.Line:
      case ChartTypes.Radar:
         return {
          fillColor: "rgba(220,220,220,0.2)",
          strokeColor: "rgba(220,220,220,1)",
          pointColor: "rgba(220,220,220,1)",
          pointStrokeColor: "#fff",
          pointHighlightFill: "#fff",
          pointHighlightStroke: "rgba(220,220,220,1)"
        };
      case ChartTypes.Bar:
        return {
          fillColor: "rgba(220,220,220,0.5)",
          strokeColor: "rgba(220,220,220,0.8)",
          highlightFill: "rgba(220,220,220,0.75)",
          highlightStroke: "rgba(220,220,220,1)"
        };
      case ChartTypes.PolarArea:
      case ChartTypes.Pie:
      case ChartTypes.Doughnut:
        return {
          color:"#F7464A",
          highlight: "#FF5A5E"
        };
    };
  }

  _getRelativeIndex(idx: NakedIndex): NakedIndex {
    let {tl} = this.props.valueRange;
    return {col: idx.col - tl.col, row: idx.row - tl.row};
  }

  // ASCell -> Bool
  _isListening(c: ASCell): boolean {
    return isContainedInLocs(c.cellLocation.index.col,
                             c.cellLocation.index.row,
                             [this.props.valueRange])
        && c.cellLocation.sheetId == this.props.sheetId;
  }

  render(): React.Element {
    let ChartConstructor = Chart[this.props.chartContext.chartType];
    return <ChartConstructor
              data={this.state.data}
              options={this.props.chartContext.options} />;
  }
}
