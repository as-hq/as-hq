import React from 'react';
import {Dialog, TextField, DropDownMenu} from 'material-ui';
import {Table, TableHeader, TableRow, TableHeaderColumn, TableBody} from 'material-ui';

import {Just, Nothing} from '../../AS/Maybe';

import {ChartTypes} from '../../Constants';
import CellStore from '../../stores/CellStore';
import ASButton from '../basic-controls/ASButton';
import ListSelector from './ASListSelector';
import ASChart from './ASChart';

export default React.createClass({

  getInitialState() {
    return {
      chartType: null
    };
  },

  _onSubmitCreate() {
    // TODO
  },

  _getInitialRange() {
    // TODO
  },

  _onChartSelectionChange(chartType) { this.setState({chartType: chartType}); },

  _onDataChange(evt) {
    // TODO
  },

  _onLabelChange(evt) {
    // TODO
  },

  _onXLabelChange(evt) {
    // TODO
  },

  dialogActions: [
    {text: 'Cancel'},
    {text: 'Create', onTouchTap: this._onSubmitCreate, ref: 'create'}
  ],

  listItems: [
    {text: 'Line', payload: ChartTypes.Line, icon: null},
    {text: 'Bar', payload: ChartTypes.Bar, icon: null},
    {text: 'Radar', payload: ChartTypes.Radar, icon: null},
    {text: 'Polar Area', payload: ChartTypes.PolarArea, icon: null},
    {text: 'Pie', payload: ChartTypes.Pie, icon: null},
    {text: 'Doughnut', payload: ChartTypes.Doughnut, icon: null},
  ],

  render() {
    return (
      <Dialog
        title="Chart Editor"
        actions={this.dialogActions}
        open={this.props.open}
        onRequestClose={this.props.onRequestClose}>

        <Table
          fixedHeader={true}>
          <TableHeader>
            <TableRow>
              <TableHeaderColumn>Chart Type</TableHeaderColumn>
              <TableHeaderColumn>Properties</TableHeaderColumn>
              <TableHeaderColumn>Preview</TableHeaderColumn>
            </TableRow>
          </TableHeader>
          <TableBody>
            <TableRow>

              <TableRowColumn>
                <TextField
                  defaultValue={this._getInitialRange()}
                  hintText="Data Range"
                  onChange={this._onDataChange} />
                <br />
                <TextField
                  hintText="Label Range"
                  onChange={this._onLabelChange} />
                <br />
                {this._isCartesian(this.state.chartType) ? (
                  [
                    <TextField
                      hintText="X Label Range"
                      onChange={this._onXLabelChange} />
                    <br />
                  ]
                ) : null}
                <ListSelector
                  items={this.listItems}
                  onSelectionChange={this._onChartSelectionChange} />
              </TableRowColumn>

              <TableRowColumn>
                <ASChart
                  chartType={this.state.chartType}
                  cells={this._getCells()}
                  xLabels={this._getXLabels()}
                  plotLabels={this._getPlotLabels()} />
              </TableRowColumn>

            </TableRow>
          </TableBody>


    );
  }

});
