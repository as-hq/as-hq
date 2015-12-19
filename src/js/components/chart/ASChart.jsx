import React from 'react';
import Chart from 'react-chartjs';

import {ChartTypes} from '../../Constants';
import CellStore from '../../stores/CellStore';
import {isContainedInLocs} from '../../AS/utils/Location';
import CU from './ChartUtils';

Chart.defaults.global.responsive = true;

export default React.createClass({

  getInitialState() {
    return { data: null };
  },

  componentDidMount() {
    CellStore.addChangeListener(this._onDataChange);
  },

  componentWillUnmount() {
    CellStore.removeChangeListener(this._onDataChange);
  },

  componentWillReceiveProps(newProps) {
    // reconstruct chart with options here
    let chartData = _contextToData(newProps.chartContext);
    let newState = {
      data: chartData,
      valueRange: newProps.valueRange,
      sheetId: newProps.sheetId,
      chartType: newProps.chartContext.chartType,
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
      let val = CU._cellToJSVal(cellValue);
      newData.datasets[col].data[row] = val;
    });
    this.setState({data: newData});
  },

  // ChartContext -> ChartData
  _contextToData(ctx) {
    if (CU._isCartesian(ctx.chartType)) {
      return {
        labels: ctx.xLabels,
        datasets: _generateCartesianDatasets(ctx)
      };
    } else if (CU._isPolar(ctx.chartType)) {
      return _generatePolarDatasets(ctx);
    }
  },

  // #needsrefactor I want an either type here.
  // the next two functions are quite repetitive, but they do return different types...

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

  _getRelativeIndex(idx) {
    let {tl} = this.props.valueRange;
    return {col: idx.col - tl.col, row: idx.row - tl.row};
  },

  // ASCell -> Bool
  _isListening(c) {
    return isContainedInLocs(c.cellLocation.index.col,
                             c.cellLocation.index.row,
                             [this.props.valueRange])
        && c.cellLocation.sheetId == this.props.sheetId;
  },

  render() {
    return <Chart[this.props.chartContext.chartType]
              data={this.state.data}
              options={this.props.chartContext.options} />;
  }
});
