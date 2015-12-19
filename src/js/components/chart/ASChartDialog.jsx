import React from 'react';
import {Dialog, TextField, DropDownMenu} from 'material-ui';
import {List, ListDivider, ListItem} from 'material-ui';
import {Table, TableHeader, TableRow, TableHeaderColumn, TableBody} from 'material-ui';

import {Just, Nothing} from '../../AS/Maybe';

import {ChartTypes} from '../../Constants';
import U from '../../AS/Util';
import CellStore from '../../stores/CellStore';
import SelStore from '../../stores/ASSelectionStore';
import ASButton from '../basic-controls/ASButton';
import ASChart from './ASChart';
import CU from './ChartUtils';

export default React.createClass({

  getInitialState() {
    return {
      valueRange: SelStore.getActiveSelection();,
      plotLabelRange: null,
      xLabelRange: null
      chartType: ChartTypes.Bar,
    };
  },

  _excelRefToVals(xp) {
    let cs = CellStore.getCells(this.state.valueRange);
    return cs.map((col) => { return col.map(CU.cellToJSVal); });
  },

  _getSourceValues() {
    let vals = this._excelRefToVals(this.refs.valueRangeInput.getValue());
    // can't polar-plot multidimensional data, so check the data
    return CU.isCartesian(this.state.chartType) ? vals : CU.reduceNestedArray(vals);
  },

  _getXLabels() {
    let xLabels = this._excelRefToVals(this.refs.xLabelRangeInput.getValue());
    let {tl, br} = this.state.valueRange;
    // polar plots don't have x-axis labels for obvious reasons
    return CU.isCartesian(this.state.chartType) ?
            (xLabels || CU.takeNat(br.row - tl.row)) :
            null;
  },

  _getPlotLabels() {
    let {tl, br} = this.state.valueRange;
    return this.state.plotLabels || CU.repeat(null, br.col - tl.col);
  },

  _generateContext() {
    return {
      chartType: this.state.chartType,
      values: this._getSourceValues(),
      xLabels: this._getXLabels(),
      plotLabels: this._getPlotLabels()
    };
  },

  _getInitialRange() { return U.Conversion.rangeToExcel(SelStore.getActiveSelection()); },

  _onSubmitCreate() {
    this.props.onCreate({
      id: U.Render.getUniqueId(),
      elem: this.refs.generatedChart,
      width: 500,
      height: 300,
      offsetX: 0,
      offsetY: 0,
      left: 50,
      top: 50,
      loc: null
    });
  },

  _onChartTypeChange(chartType) { this.setState({chartType: chartType}); },

// kind of repetitive, these event listeners...
  _onSourceChange() {
    let str = this.refs.valueRangeInput.getValue();
    if (U.parsing.isValidExcelRef(str)) {
      let rng = U.conversion.excelToRange(str);
      this.setState({valueRange: rng});
    } else {
      this.refs.valueRangeInput.errorText = "Please enter a valid A1:B2 style reference."
    }
  },

  _onLabelChange() {
    let str = this.refs.labelRangeInput.getValue();
    if (U.parsing.isValidExcelRef(str)) {
      let rng = U.conversion.excelToRange(str);
      this.setState({plotLabelRange: rng});
    } else {
      this.refs.labelRangeInput.errorText = "Please enter a valid A1:B2 style reference."
    }
  },

  _onXLabelChange() {
    let str = this.refs.xLabelRangeInput.getValue();
    if (U.parsing.isValidExcelRef(str)) {
      let rng = U.conversion.excelToRange(str);
      this.setState({xLabelRange: rng});
    } else {
      this.refs.xLabelRangeInput.errorText = "Please enter a valid A1:B2 style reference."
    }
  },

  dialogActions: [
    {text: 'Cancel'},
    {text: 'Create', onTouchTap: this._onSubmitCreate}
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
              <TableHeaderColumn>Options</TableHeaderColumn>
              <TableHeaderColumn>Preview</TableHeaderColumn>
            </TableRow>
          </TableHeader>
          <TableBody>
            <TableRow>

              <TableRowColumn>
                <TextField
                  ref="valueRangeInput"
                  defaultValue={this._getInitialRange()}
                  hintText="Data Range"
                  onChange={this._onSourceChange()} />
                <br />
                <TextField
                  ref="labelRangeInput"
                  hintText="Dataset Label Range"
                  errorStyle={{color:'orange'}}
                  onChange={this._onLabelChange()} />
                <br />
                {CU.isCartesian(this.state.chartType) ? (
                  [
                    <TextField
                      ref="xLabelRangeInput"
                      hintText="X-axis Label Range"
                      errorStyle={{color:'orange'}}
                      onChange={this._onXLabelChange()} />
                    <br />
                  ]
                ) : null}

                <SelectableList
                  subheader="Chart types"
                  valueLink={{value: this.state.chartType, requestChange: this._onChartTypeChange}} >
                  {this.listItems.map((item) => {
                    return (
                      <ListItem
                        primaryText={item.text}
                        leftIcon={item.icon}
                        value={item.payload} />
                      <ListDivider inset={true} />
                    );
                  })}
                </SelectableList>
              </TableRowColumn>

              <TableRowColumn>
                <ASChart
                  ref="generatedChart"
                  valueRange={this.state.valueRange}
                  sheetId={CellStore.getCurrentSheet().sheetId}
                  chartContext={this._generateContext()} />
              </TableRowColumn>

            </TableRow>
          </TableBody>


    );
  }

});
