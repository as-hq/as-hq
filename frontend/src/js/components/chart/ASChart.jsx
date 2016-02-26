/* @flow */

import type {
  Style
} from '../../types/Render';

import type {
  ASChartType,
  ASChartContext,
  ASChartData,
  CartesianDataset,
  PolarDataset
} from './types';

import React from 'react';
// $FlowFixMe too lazy to declare this import rn
import Chart from 'react-chartjs';

import Constants from '../../Constants';
import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';
import U from '../../AS/Util';
import CU from './ChartUtils';

import ASCell from '../../classes/ASCell';
import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';

let {ChartTypes} = Constants;

type ASChartProps = {
  chartContext: ASChartContext;
  chartStyle: Style;
  valueRange: ASRange;
  sheetId: string;
  redraw: boolean;
  showLegend: boolean;
};

type ASChartState = {
  data: ASChartData;
};

export default class ASChart extends React.Component<{}, ASChartProps, ASChartState> {

  _cellStoreToken: { remove: () => void };

  constructor(props: ASChartProps) {
    super(props);

    this.state = {
      data: this._contextToData(this.props.chartContext),
    };
  }

  componentDidMount() {
    // TODO (anand) fix
    // this._cellStoreToken = CellStore.addListener(() => this._onDataChange());
  }

  componentWillUnmount() {
    // TODO (anand) fix
    // this._cellStoreToken.remove();
  }

  componentWillReceiveProps(newProps: ASChartProps) {
    let {chartContext} = newProps;
    if (chartContext !== this.props.chartContext) {
      let chartData = this._contextToData(chartContext);
      this.setState({data: chartData});
    }
  }

  _getChart(): Chart {
    return this.refs.baseChart.getChart();
  }

  _isListening({location}: ASCell): boolean {
    return this.props.valueRange.contains(location)
      && location.sheetId === this.props.sheetId;
  }

  // NOTE: there is no longer a concept of 'lastUpdatedCells' in
  // ASCellStore, so this method is deprecated for now. Anand 2/23
  _onDataChange() {
    throw new Error('charts not implemented for new CellStore!');
    // let filteredCells = CellStore.getLastUpdatedCells().filter(this._isListening.bind(this));
    // this._updateData(filteredCells);
  }

  _getRelativeIndex({col, row}: ASIndex): ({ dr: number; dc: number; }) {
    let {tl} = this.props.valueRange;
    return {dc: col - tl.col, dr: row - tl.row};
  }

  _updateData(cs: Array<ASCell>) {
    let newData = this.state.data;
    cs.forEach((c) => {
      let {dc, dr} = this._getRelativeIndex(c.location);
      let val = CU.cellToChartVal(c);
      // update the datastructure depending on the chart type
      if (CU.isCartesian(this.props.chartContext.chartType) && newData.datasets) {
        newData.datasets[dc].data[dr] = val;
      } else {
        let insertIdx = Math.max(dc, dr);
        newData[insertIdx].value = val;
      }
    });
    this.setState({data: newData});
  }

  // ChartContext -> ChartData
  _contextToData(ctx: ASChartContext): ASChartData {
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
    let {r,g,b} = CU.generateRGB();
    switch(chartType) {
      case ChartTypes.Line:
      case ChartTypes.Radar:
         return {
          fillColor: `rgba(${r},${g},${b},0.2)`,
          strokeColor: `rgba(${r},${g},${b},1)`,
          pointColor: `rgba(${r},${g},${b},1)`,
          pointStrokeColor: "#fff",
          pointHighlightFill: "#fff",
          pointHighlightStroke: `rgba(${r},${g},${b},1)`
        };
      case ChartTypes.Bar:
        return {
          fillColor: `rgba(${r},${g},${b},0.9)`,
          strokeColor: `rgba(${r},${g},${b},0.8)`,
          highlightFill: `rgba(${r},${g},${b},0.75)`,
          highlightStroke: `rgba(${r},${g},${b},1)`
        };
      case ChartTypes.PolarArea:
      case ChartTypes.Pie:
      case ChartTypes.Doughnut:
        return {
          color:`rgba(${r},${g},${b},0.8)`,
          highlight: `rgba(${r},${g},${b},0.5)`
        };
    };
  }

// not necessary until legends need to work.

  // _generateLegendMarkup(): string {
  //   let legend = this._getChart().generateLegend();
  //   return {__html: legend};
  // }
  //
  // _isMounted(): boolean {
  //   return (!! this.refs.baseChart);
  // }

  render(): React.Element {
    let ChartConstructor = Chart[this.props.chartContext.chartType];
    let {data} = this.state;
    let {redraw, chartContext, chartStyle, showLegend} = this.props;

    return (
        <ChartConstructor
          ref='baseChart'
          style={chartStyle}
          data={data}
          options={chartContext.options}
          redraw={redraw} />
    );
      // I can't get the CSS for the legend to work,
      // so 80/20.

        // {showLegend && this._isMounted() ?
        //   <div
        //     dangerouslySetInnerHTML={this._generateLegendMarkup()}
        //     style={{
        //       position: 'absolute',
        //       top: '10px',
        //       left: '10px',
        //       zIndex: 10
        //     }} />
        //   : []
        // }
  }
}
