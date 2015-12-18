import React from 'react';
import Chart from 'react-chartjs';

import {ChartTypes} from '../Constants';
import CellStore from '../stores/CellStore';
import {isContainedInLocs} from '../AS/utils/Location';

Chart.defaults.global.responsive = true;

export default React.createClass({

  getInitialState() {
    return {
      data: null,
      range: null,
      sheetId: null,
      chartType: null,
      chartOptions: null
    }
  },

  componentDidMount() {
    CellStore.addChangeListener(this._onDataChange);
  },

  componentWillUnmount() {
    CellStore.removeChangeListener(this._onDataChange);
  },

  componentWillReceiveProps(newProps) {
    // reconstruct chart with options here
    let chartContext = _generateContext(newProps.chartType, newProps.cells, newProps.xLabels, newProps.plotLabels);
    let chartData = _contextToData(chartContext);
    let newState = {
      data: chartData,
      range: newProps.range,
      sheetId: newProps.sheetId,
      chartType: newProps.chartType,
    };
    this.replaceState(newState);
  },

  // data binding
  _onDataChange() {
    let filteredCells = CellStore.getLastUpdatedCells().filter(_isListening);
    this._updateData(filteredCells);
  },

  _updateData(cs) {
    let newData = this.state.data;
    cs.forEach((c) => {
      let {col, row} = _getRelativeIndex(c.cellLocation.index);
      let val = _cellToJSVal(cellValue);
      newData.datasets[col].data[row] = val;
    });
    this.setState({data: newData});
  },

  // ChartType -> [[ASCell]] -> Maybe [XLabel] -> Maybe [PlotLabel] -> ChartContext
  _generateContext(chartType, cs, xLabels, plotLabels) {
    if _isCartesian(chartType) {
      return {
        chartType: chartType,
        values: cs.map((col) => { return col.map(_cellToJSVal); }),
        xLabels: xLabels || _takeNat(cs[0].length),
        plotLabels: plotLabels || _repeat(null, cs.length)
      };
    } else if _isPolar(chartType) {
      return {
        chartType: chartType,
        // can't polar plot multi-dimensional data, so reduce the array.
        values: _reduceNestedArray(cs).map(_cellToJSVal),
        plotLabels: plotLabels || _repeat(null, cs.length)
      };
    }
  },

  // ChartContext -> ChartData
  _contextToData(ctx) {
    if (_isCartesian(ctx.chartType)) {
      return {
        labels: ctx.xLabels,
        datasets: _generateCartesianDatasets(ctx)
      };
    } else if (_isPolar(ctx.chartType)) {
      return _generatePolarDatasets(ctx);
    }
  },

  // ChartContext -> [CartesianDataset]
  _generateCartesianDatasets(ctx) {
    let datasets = Array(ctx.values.length);
    for (var i=0; i<ctx.values.length; i++) {
      datasets[i] = _generateDatasetGraphicOptions(ctx.chartType)
        .assign({
          label: ctx.plotLabels[i],
          data: ctx.values[i]
        });
    } return datasets;
  },

  // ChartContext -> [PolarDataset]
  _generatePolarDatasets(ctx) {
    let datasets = Array(ctx.values.length);
    for (var i=0; i<ctx.values.length; i++) {
      datasets[i] = _generateDatasetGraphicOptions(ctx.chartType)
        .assign({
          label: ctx.plotLabels[i],
          value: ctx.values[i]
        });
    } return datasets;
  },

  // ChartType -> GraphicOptions
  _generateDatasetGraphicOptions(chartType) {
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
  },

  _isCartesian(chartType) { return [ChartTypes.Line, ChartTypes.Bar, ChartTypes.Radar].includes(chartType); },

  _isPolar(chartType) { return [ChartTypes.PolarArea, ChartTypes.Pie, ChartTypes.Doughnut].includes(chartType); },

  _getRelativeIndex(idx) {
    let {tl} = this.state.range;
    return {col: idx.col - tl.col, row: idx.row - tl.row};
  },

  // a -> Int -> [a]
  _repeat(val, n) {
    return Array(...Array(n)).map(() => val);
  },

  // [[a]] -> ThrowsError a
  _reduceNestedArray(arr) {
    console.assert(arr.constructor === Array && arr.every((elem) => { return elem.constructor === Array; }));
    console.assert(arr.length == 1 || arr.every((subArr) => { return subArr.length == 1; }));
    if (arr.length == 1) return arr[0];
    else return arr.map((elem) => { return elem[0]; });
  },

  // ASCell -> Maybe JSVal
  _cellToJSVal(c) {
    switch (c.cellValue.tag) {
      case "ValueI":
      case "ValueD":
      case "ValueB":
        return c.cellValue.contents;
      default:
        console.error("Cannot chart non-numeric value.");
        return null;
    };
  },

  // ASCell -> Bool
  _isListening(c) {
    return isContainedInLocs(c.cellLocation.index.col,
                             c.cellLocation.index.row,
                             [this.state.range])
        && c.cellLocation.sheetId == this.state.sheetId;
  },

  setChartType(t) { this.setState({chartType: t}); },

  render() {
    return <Chart[this.state.chartType] data={this.state.data} options={this.state.chartOptions} />;
  }
});
