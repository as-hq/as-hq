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
  valueRange: ?NakedRange;
  plotLabelRange: ?NakedRange;
  xLabelRange: ?NakedRange;
  chartType: ASChartType;
  errorMessages: {
    valueRangeInput: ?string,
    plotLabelRangeInput: ?string,
    xLabelRangeInput: ?string
  }
};

const LIST_ITEMS = [
  {text: 'Bar', payload: 'Bar', icon: null},
  {text: 'Line', payload: 'Line', icon: null},
  {text: 'Radar', payload: 'Radar', icon: null},
  {text: 'Polar Area', payload: 'PolarArea', icon: null},
  {text: 'Pie', payload: 'Pie', icon: null},
  {text: 'Doughnut', payload: 'Doughnut', icon: null},
];

export default class ASChartDialog extends React.Component<{}, ASChartDialogProps, ASChartDialogState> {
  constructor(props: ASChartDialogProps) {
    super(props);

    this.state = {
      valueRange: null,
      plotLabelRange: null,
      xLabelRange: null,
      chartType: 'Bar',
      errorMessages: {
        valueRangeInput: null,
        plotLabelRangeInput: null,
        xLabelRangeInput: null
      }
    };
  }

  componentWillReceiveProps(newProps: ASChartDialogProps) {
    if (newProps.open) {
      let sel = SelStore.getActiveSelection();
      if (sel !== null && sel !== undefined) {
        this.setState({valueRange: sel.range});
      }
    };
  }

  _getSourceValues(): ?(ASCartesianValues | ASPolarValues) {
    console.log("valuerange in _getSourceValues:" + JSON.stringify(this.state.valueRange));
    let {valueRange, chartType} = this.state;
    if (valueRange) {
      let vals = CellStore.getCells(valueRange)
                    .map((cs) => cs.map(CU.cellToChartVal));
      // can't polar-plot multidimensional data, so check the data
      return CU.isCartesian(chartType) ? vals : CU.reduceNestedArrayNum(vals);
    } else { return null; }
  }

  _getXLabels(): ?Array<string> {
    let {valueRange, xLabelRange} = this.state;
    if (xLabelRange) {
      let xLabels = CellStore.getCells(xLabelRange)
        .map((cs) => cs.map(CU.cellToLabel));
      // polar plots don't have x-axis labels for obvious reasons
      return CU.isCartesian(this.state.chartType) ? CU.reduceNestedArrayStr(xLabels) : null;
    } else if (valueRange) {
      let {tl, br} = valueRange;
      return CU.generateXLabels(br.row - tl.row + 1);
    } else { return null; }
  }

  _getPlotLabels(): ?Array<string> {
    let {plotLabelRange, valueRange} = this.state;
    if (plotLabelRange) {
      let plotLabels = CellStore.getCells(plotLabelRange)
        .map((cs) => cs.map(CU.cellToLabel));

      return CU.reduceNestedArrayStr(plotLabels);
    } else if (valueRange) {
      console.log("Generating plot labels");
      let {tl, br} = valueRange;
      return CU.generatePlotLabels(br.col - tl.col + 1);
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

  _onChartTypeChange(e: SyntheticEvent, chartType: ASChartType) {
    this.setState({chartType: chartType});
  }

  _onRangeInputChange(ref: string, stateField: string): Callback {
    return () => {
      if (this.refs[ref]) {
        let str = this.refs[ref].getValue();
        if (U.Parsing.isValidExcelRef(str)) {
          let rng = U.Conversion.excelToRange(str);
          let newState = {};
          newState[stateField] = rng;
          this.setState(newState);
          this._setErrorText(ref, null);
        } else {
          this._setErrorText(ref, "Please enter a valid A1:B2 style reference.");
        }
      }
    };
  }

  _setErrorText(inputRef: string, msg: ?string) {
    let msgs = this.state.errorMessages;
    msgs[inputRef] = msg;
    this.setState({errorMessages: msgs});
  }

  _checkConfiguration() {
    let {valueRange, chartType} = this.state;
    let failCallbacks = [
      [valueRange === null, () => {console.error("the data range is null.")}],
      [CU.isPolar(chartType) && !CU.isVector(valueRange), () => {this._setErrorText("valueRangeInput", "Please enter a one-dimensional range for Polar charts.")}]
    ]
  }

  render(): React.Element {
    let {open, onRequestClose} = this.props;
    let {chartType, valueRange} = this.state;
    let shouldRenderPreview = this._checkConfiguration();

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
                onChange={this._onRangeInputChange("valueRangeInput", "valueRange").bind(this)}
                style={_Styles.inputs} />
              <br />
              <TextField
                ref="plotLabelRangeInput"
                hintText="Dataset Label Range"
                errorStyle={{color:'orange'}}
                onChange={this._onRangeInputChange("plotLabelRangeInput", "plotLabelRange").bind(this)}
                style={_Styles.inputs} />
              <br />
              {CU.isCartesian(chartType) ? (
                [
                  <TextField
                    ref="xLabelRangeInput"
                    hintText="X-axis Label Range"
                    errorStyle={{color:'orange'}}
                    onChange={this._onRangeInputChange("xLabelRangeInput", "xLabelRange").bind(this)}
                    style={_Styles.inputs} />,
                  <br />
                ]
              ) : null}
            </td>
            <td>
              {shouldRenderPreview ? (
                [
                  <ASChart
                    ref="generatedChart"
                    valueRange={valueRange}
                    sheetId={SheetStore.getCurrentSheet().sheetId}
                    chartContext={this._generateContext()} />
                ]
              ) : "No selection."}
            </td>
          </tr>
          <tr colSpan="1">
            <td>
              <SelectableList
                subheader="Chart types"
                valueLink={{value: chartType, requestChange: this._onChartTypeChange.bind(this)}} >
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
