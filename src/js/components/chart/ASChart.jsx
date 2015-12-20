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
  ASChartData,
  CartesianDataset,
  PolarDataset
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
  data: ASChartData;
  valueRange: NakedRange;
  sheetId: string;
};

export default class ASChart extends React.Component<{}, ASChartProps, ASChartState> {
  constructor(props: ASChartProps) {
    super(props);

    this.state = {
      data: this._contextToData(this.props.chartContext),
      valueRange: props.valueRange,
      sheetId: props.sheetId
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
      sheetId: newProps.sheetId
    };
    this.setState(newState);
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
      let val = CU.cellToChartVal(c);
      // cartesian data case
      if (newData.datasets !== null && newData.datasets !== undefined) {
        newData.datasets[col].data[row] = val;
      } else if (newData instanceof Array) {
        let insertIdx = Math.max(col, row);
        newData[col].value = val;
      }
    });
    this.setState({data: newData});
  }

  // ChartContext -> ChartData
  _contextToData(ctx: ASChartContext): ASChartData {
    console.log("Contexttodata: " + JSON.stringify(ctx));
    const {xLabels} = ctx;
    if (CU.isCartesian(ctx.chartType) && xLabels !== null && xLabels !== undefined) {
      return {
        labels: xLabels,
        datasets: this._generateCartesianDatasets(ctx)
      };
    } else if (CU.isPolar(ctx.chartType)) {
      return this._generatePolarDatasets(ctx);
    } else throw new Error("Not even cartesian or polar: " + JSON.stringify(ctx.chartType));
  }

  // #needsrefactor I want an either type here.
  // the next two functions are quite repetitive, but they do return different types...

  // ChartContext -> [CartesianDataset]
  _generateCartesianDatasets(ctx: ASChartContext): Array<CartesianDataset> {
    let datasets = [];
    const {values, plotLabels} = ctx;
    if (values !== null && values !== undefined && plotLabels !== null && plotLabels !== undefined) {
      for (var i = 0; i < values.length; i++) {
        // $FlowFixMe can't declare object.assign...
        datasets.push(Object.assign(this._generateDatasetGraphicOptions(ctx.chartType), {
            label: plotLabels[i],
            data: values[i]
          }
        ));
      } return datasets;
    } else throw new Error("Couldn't generate cartesian datasets");
  }

  // ChartContext -> [PolarDataset]
  _generatePolarDatasets(ctx: ASChartContext): Array<PolarDataset> {
    console.log("generating polar datasets with context: " + JSON.stringify(ctx));
    let datasets = [];
    const {values, plotLabels} = ctx;
    if (values !== null && values !== undefined && plotLabels !== null && plotLabels !== undefined) {
      for (var i=0; i<values.length; i++) {
        // $FlowFixMe fuck you object.assign
        datasets.push(Object.assign(this._generateDatasetGraphicOptions(ctx.chartType), {
            label: plotLabels[i],
            value: values[i]
          }
        ));
      } return datasets;
    } else throw new Error("Couldn't generate polar datasets");
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
    let {index, sheetId} = c.cellLocation;
    return isContainedInLocs(index.col, index.row, [this.props.valueRange])
        && sheetId == this.props.sheetId;
  }

  render(): React.Element {
    let ChartConstructor = Chart[this.props.chartContext.chartType];
    return <ChartConstructor
              data={this.state.data}
              options={this.props.chartContext.options} />;
  }
}
