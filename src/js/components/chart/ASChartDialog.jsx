/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  NakedRange,
  ASCell
} from '../../types/Eval';

import type {
  ASOverlaySpec
} from '../../types/Hypergrid';

import type {
  ASChartType,
  ASCartesianChartType,
  ASPolarChartType,
  ASChartContext,
  ASPolarValues,
  ASCartesianValues
} from './types';

import React from 'react';
import {Dialog, TextField, DropDownMenu} from 'material-ui';
// $FlowFixMe: Too lazy to add ~14 things to declarations
import {List, Divider, ListItem} from 'material-ui';

import {Just, Nothing} from '../../AS/Maybe';

import Constants from '../../Constants';
import _Styles from '../../styles/chart/ASChartDialog';
import U from '../../AS/Util';
import CellStore from '../../stores/ASCellStore';
import SelStore from '../../stores/ASSelectionStore';
import SheetStore from '../../stores/ASSheetStateStore';
import ASButton from '../basic-controls/ASButton.jsx';
import ASChart from './ASChart.jsx';
import CU from './ChartUtils';

let {ChartTypes} = Constants;

import { SelectableContainerEnhance } from 'material-ui/lib/hoc/selectable-enhance';
let SelectableList = SelectableContainerEnhance(List);

type ASChartDialogProps = {
  onCreate: (ele: ASOverlaySpec) => void;
  open: bool;
  onRequestClose: Callback;
};

type ASChartDialogState = {
  valueRange: NakedRange;
  plotLabelRange: ?NakedRange;
  xLabelRange: ?NakedRange;
  chartType: ASChartType;
  chartTypeIndex: number;
};

const LIST_ITEMS = [
  {text: 'Line', payload: 'Line', icon: null},
  {text: 'Bar', payload: 'Bar', icon: null},
  {text: 'Radar', payload: 'Radar', icon: null},
  {text: 'Polar Area', payload: 'PolarArea', icon: null},
  {text: 'Pie', payload: 'Pie', icon: null},
  {text: 'Doughnut', payload: 'Doughnut', icon: null},
];

export default class ASChartDialog extends React.Component<{}, ASChartDialogProps, ASChartDialogState> {
  constructor(props: ASChartDialogProps) {
    super(props);

    this.state = {
      chartTypeIndex: 1,
      valueRange: {tl: {col: 1, row: 1}, br: {col: 1, row: 1}},
      plotLabelRange: null,
      xLabelRange: null,
      chartType: 'Bar'
    };
  }

  _getSourceValues(): ?(ASCartesianValues | ASPolarValues) {
    if (this.state.valueRange) {
       let vals = CellStore.getCells(this.state.valueRange)
                    .map((cs) => cs.map(CU.cellToChartVal));
      // can't polar-plot multidimensional data, so check the data
      return CU.isCartesian(this.state.chartType) ? vals : CU.reduceNestedArrayNum(vals);
    } else { return null; }
  }

  _getXLabels(): ?Array<string> {
    if (this.state.xLabelRange) {
      let xLabels = CellStore.getCells(this.state.xLabelRange)
        .map((cs) => cs.map(CU.cellToLabel));
      let {tl, br} = this.state.valueRange;

      if (!(tl.row === br.row || tl.col === br.col)) {
        throw new Error('Not a one-dimensional range');
      }

      // polar plots don't have x-axis labels for obvious reasons
      return CU.isCartesian(this.state.chartType) ?
              (CU.reduceNestedArrayStr(xLabels) || CU.takeNat(br.row - tl.row).map((n) => n.toString())) :
              null;
    } else { return null; }
  }

  _getPlotLabels(): ?Array<string> {
    if (this.state.plotLabelRange) {
      let plotLabels = CellStore.getCells(this.state.plotLabelRange)
        .map((cs) => cs.map(CU.cellToLabel));
      let {tl, br} = this.state.valueRange;

      if (!(tl.row === br.row || tl.col === br.col)) {
        throw new Error('Not a one-dimensional range');
      }

      return CU.reduceNestedArrayStr(plotLabels) || CU.repeat(null, br.col - tl.col);
    } else { return null; }
  }

  _generateContext(): ASChartContext {
    let ctx = {
      chartType: this.state.chartType,
      values: this._getSourceValues(),
      xLabels: this._getXLabels(),
      plotLabels: this._getPlotLabels(),
      options: {} // no options for now
    };
    console.log("Generate context: \n\n" + JSON.stringify(ctx));
    return ctx;
  }

  _getInitialRangeExpression(): string {
    let sel = SelStore.getActiveSelection();
    if (!! sel){ return U.Conversion.rangeToExcel(sel.range); }
    else { return ''; }
  }

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
  }

  _onChartTypeChange(chartType: ASChartType, idx: number) {
    this.setState({chartType: chartType, chartTypeIndex: idx});
  }

  _onSourceChange() {
    if (this.refs.valueRangeInput) {
      let str = this.refs.valueRangeInput.getValue();
      if (U.Parsing.isValidExcelRef(str)) {
        let rng = U.Conversion.excelToRange(str);
        this.setState({valueRange: rng});
      } else {
        this.refs.valueRangeInput.errorText = "Please enter a valid A1:B2 style reference.";
      }
    }
  }

  _onLabelChange() {
    if (this.refs.labelRangeInput) {
      let str = this.refs.labelRangeInput.getValue();
      if (U.Parsing.isValidExcelRef(str)) {
        let rng = U.Conversion.excelToRange(str);
        this.setState({plotLabelRange: rng});
      } else {
        this.refs.labelRangeInput.errorText = "Please enter a valid A1:B2 style reference.";
      }
    }
  }

  _onXLabelChange() {
    if (this.refs.xLabelRangeInput) {
      let str = this.refs.xLabelRangeInput.getValue();
      if (U.Parsing.isValidExcelRef(str)) {
        let rng = U.Conversion.excelToRange(str);
        this.setState({xLabelRange: rng});
      } else {
        this.refs.xLabelRangeInput.errorText = "Please enter a valid A1:B2 style reference.";
      }
    }
  }

  render(): React.Element {
    let {open, onRequestClose} = this.props;
    let {chartType, valueRange} = this.state;

    return (
      <Dialog
        title="Chart Editor"
        actions={[
          {text: 'Cancel'},
          {text: 'Create', onTouchTap: this._onSubmitCreate.bind(this)}
        ]}
        open={open}
        onRequestClose={onRequestClose}>

        <table>
          <tr>
            <th>Options</th>
            <th>Preview</th>
          </tr>
          <tr>
            <td colSpan="1">
              <TextField
                ref="valueRangeInput"
                defaultValue={this._getInitialRangeExpression()}
                hintText="Data Range"
                onChange={this._onSourceChange.bind(this)}
                style={_Styles.inputs} />
              <br />
              <TextField
                ref="labelRangeInput"
                hintText="Dataset Label Range"
                errorStyle={{color:'orange'}}
                onChange={this._onLabelChange.bind(this)}
                style={_Styles.inputs} />
              <br />
              {CU.isCartesian(chartType) ? (
                [
                  <TextField
                    ref="xLabelRangeInput"
                    hintText="X-axis Label Range"
                    errorStyle={{color:'orange'}}
                    onChange={this._onXLabelChange.bind(this)}
                    style={_Styles.inputs} />,
                  <br />
                ]
              ) : null}
            </td>
            <td>
              <ASChart
                ref="generatedChart"
                valueRange={valueRange}
                sheetId={SheetStore.getCurrentSheet().sheetId}
                chartContext={this._generateContext()} />
            </td>
          </tr>
          <tr colSpan="1">
            <td>
              <SelectableList
                subheader="Chart types"
                valueLink={{value: this.state.chartTypeIndex, requestChange: this._onChartTypeChange.bind(this)}} >
                {LIST_ITEMS.map((item) => {
                  return ([
                    <ListItem
                      primaryText={item.text}
                      leftIcon={item.icon}
                      value={item.payload} />,
                    <Divider inset={true} />
                  ]);
                })}
              </SelectableList>
            </td>
          </tr>
        </table>
      </Dialog>
    );
  }

}
