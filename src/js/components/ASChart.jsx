import React from 'react';
import Chart from 'react-chartjs';

import {ChartTypes} from '../Constants';
import CellStore from '../stores/CellStore';
import {isContainedInLocs} from '../AS/utils/Location';

Chart.defaults.global.responsive = true;

export default React.createClass({

  getInitialState() {
    return {
      data: {
        labels: [],
        datasets: []
      },
      range: null,
      sheetId: null,
      chartType: null,
      chartOptions: null
    }
  }

  componentDidMount() {
    CellStore.addChangeListener(this._onDataChange);
  },

  componentWillUnmount() {
    CellStore.removeChangeListener(this._onDataChange);
  },

  componentWillReceiveProps(newProps) {
    // reconstruct chart with options here
    // TODO
  },

  // data binding
  _onDataChange() {
    let filteredCells = CellStore.getLastUpdatedCells().filter(isListening);
    this._insertCells(filteredCells);
  },

  _insertCells(cs) {
    let newData = this.state.data;
    cs.forEach((c) => {
      let {col, row} = getRelativeIndex(c.cellLocation.index);
      let val = ASToJSVal(c.cellValue);
      newData.datasets[col].data[row] = val;
    });
    this.setState({data: newData});
  },

  getRelativeIndex(idx) {
    let {tl} = this.state.range;
    return {col: idx.col - tl.col, row: idx.row - tl.row};
  },

  ASToJSVal(cv) {
    switch (cv.tag) {
      case "ValueI":
      case "ValueD":
      case "ValueB":
      case "ValueS":
        return cv.contents;
      default:
        // throw new Error("cannot chart non-primitive value");
        return null;
    };
  },

  isListening(c) {
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
