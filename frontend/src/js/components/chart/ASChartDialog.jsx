/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  ASOverlaySpec
} from '../../types/Hypergrid';

import type {
  ASChartType,
  ASCartesianChartType,
  ASPolarChartType,
  ASChartContext,
  ASPolarValues,
  ASCartesianValues,
  ASChartOptions
} from './types';

import React from 'react';
import ReactDOM from 'react-dom';
import {Dialog, TextField, DropDownMenu} from 'material-ui';
// $FlowFixMe: Too lazy to add ~14 things to declarations
import {List, Divider, ListItem} from 'material-ui';
// $FlowFixMe: Too lazy to add ~14 things to declarations
import {Toggle} from 'material-ui';
// $FlowFixMe: Too lazy to add ~14 things to declarations
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe: Too lazy to add ~14 things to declarations
import LightTheme from 'material-ui/lib/styles/raw-themes/light-raw-theme';

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

import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';

// $FlowFixMe: Too lazy to add ~14 things to declarations
import { SelectableContainerEnhance } from 'material-ui/lib/hoc/selectable-enhance';
let SelectableList = SelectableContainerEnhance(List);

type ChartDialogRef = "valueRangeInput" | "plotLabelRangeInput" | "xLabelRangeInput";
type ErrorMessages = { [key: ChartDialogRef]: string };

type ASChartDialogProps = {
  onCreate: (ele: ASOverlaySpec) => void;
  open: bool;
  onRequestClose: Callback;
};

type ASChartDialogState = {
  valueRange: ?ASRange;
  plotLabelRange: ?ASRange;
  xLabelRange: ?ASRange;
  chartType: ASChartType;
  showLegend: boolean;
};


const LIST_ITEMS = [
  {text: 'Bar', payload: 'Bar', icon: null},
  {text: 'Line', payload: 'Line', icon: null},
  {text: 'Radar', payload: 'Radar', icon: null},
  {text: 'Polar Area', payload: 'PolarArea', icon: null},
  {text: 'Pie', payload: 'Pie', icon: null},
  {text: 'Doughnut', payload: 'Doughnut', icon: null},
];

class ASChartDialog extends React.Component<{}, ASChartDialogProps, ASChartDialogState> {
  constructor(props: ASChartDialogProps) {
    super(props);

    this.state = {
      valueRange: null,
      plotLabelRange: null,
      xLabelRange: null,
      chartType: 'Bar',
      showLegend: true
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

  getChildContext(): any {
    return {
      muiTheme: ThemeManager.getMuiTheme(LightTheme)
    };
  }

  _getSourceValues(): ?(ASCartesianValues | ASPolarValues) {
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
      let plotLabels = U.Array.map2d(
        CellStore.getCells(plotLabelRange),
        CU.cellToLabel
      );

      return CU.reduceNestedArrayStr(plotLabels);
    } else if (valueRange) {
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
    return ctx;
  }

  _getInitialRangeExpression(): string {
    let sel = SelStore.getActiveSelection();
    if (!! sel){ return sel.range.toExcel().toString(); }
    else { return ''; }
  }

  _onSubmitCreate() {
    const {valueRange, showLegend} = this.state;
    let errorMessages = this._checkConfigurationErrors();
    let hasError = Object.keys(errorMessages).length > 0;
    if (valueRange !== null && valueRange !== undefined && !hasError) {
      let ctx = this._generateContext();
      let sheetId = SheetStore.getCurrentSheetId();
      this.props.onCreate({
        id: U.Render.getUniqueId(),
        renderElem: (style) =>
          { return (<ASChart
                      ref="chart"
                      valueRange={valueRange}
                      sheetId={sheetId}
                      chartContext={ctx}
                      chartStyle={style}
                      showLegend={showLegend}
                      redraw={false} />); },
        initWidth: 500,
        initHeight: 300,
        offsetX: 0,
        offsetY: 0,
        left: 50,
        top: 50,
        loc: null
      });
      this.props.onRequestClose();
    }
  }

  _onChartTypeChange(e: SyntheticEvent, chartType: ASChartType) {
    this.setState({chartType: chartType});
  }

  _onRangeInputChange(ref: ChartDialogRef, stateField: string) {
    if (this.refs[ref]) {
      let str = this.refs[ref].getValue();
      let newState = {};
      if (U.Parsing.isFiniteExcelRef(str)) { // doesn't work with A:A right now, because that's not doable on frontend.
        let rng = ASRange.fromExcelString(str);
        newState[stateField] = rng;
      } else {
        newState[stateField] = null;
      }
      this.setState(newState);
    }
  }

  _onToggleLegend(value: boolean) {
    this.setState({showLegend: value});
  }

  _checkConfigurationErrors(): ErrorMessages {
    let {valueRange, chartType} = this.state;
    let errorMessages = {};

    // check for certain conditions
    if (valueRange === null || valueRange === undefined)
      { errorMessages['valueRangeInput'] = "No selection."; }
    else if (CU.isPolar(chartType) && !CU.isVectorReference(valueRange))
      { errorMessages['valueRangeInput'] = "Please enter a one-dimensional range for polar charts."; }

    // text field sanitization
    ['valueRangeInput', 'plotLabelRangeInput', 'xLabelRangeInput']
      .forEach((ref) => {
        if (this.refs[ref]) {
          let inputValue = this.refs[ref].getValue();
          if (!U.Parsing.isFiniteExcelRef(inputValue) && !U.Parsing.isWhitespace(inputValue))
            { errorMessages[ref] = "Please enter a valid A1:B2 style reference."; }
        }
    });

    return errorMessages;
  }

  render(): React.Element {
    let {open, onRequestClose} = this.props;
    let {chartType, valueRange, showLegend} = this.state;
    let errorMessages = this._checkConfigurationErrors();
    let shouldRenderPreview = Object.keys(errorMessages).length == 0;
    // let ChartLegend = this.refs.generatedChart ? this.refs.generatedChart.generateLegend() : <div />;

    return (
      <Dialog
        title="Chart Editor"
        actions={[
          {text: 'Cancel'},
          {text: 'Create', onTouchTap: () => this._onSubmitCreate()}
        ]}
        open={open}
        onRequestClose={onRequestClose} >

        <div style={_Styles.settingsPanel}>
          <TextField
            ref="valueRangeInput"
            defaultValue={this._getInitialRangeExpression()}
            hintText="Data Range"
            errorText={errorMessages.valueRangeInput || ''}
            onChange={() => this._onRangeInputChange("valueRangeInput", "valueRange")}
            style={_Styles.inputs} />
          <br />

          <TextField
            ref="plotLabelRangeInput"
            hintText="Dataset Label Range"
            errorText={errorMessages.plotLabelRangeInput || ''}
            errorStyle={{color:'orange'}}
            onChange={() => this._onRangeInputChange("plotLabelRangeInput", "plotLabelRange")}
            style={_Styles.inputs} />

          <br />

          {CU.isCartesian(chartType) ? (
            [
              <TextField
                ref="xLabelRangeInput"
                hintText="X-axis Label Range"
                errorText={errorMessages.xLabelRangeInput || ''}
                errorStyle={{color:'orange'}}
                onChange={() => this._onRangeInputChange("xLabelRangeInput", "xLabelRange")}
                style={_Styles.inputs} />,
              <br />
            ]
          ) : null}

          <br />

          <Toggle
            label="Show legend"
            defaultToggled={true}
            onToggled={value => this._onToggleLegend(value)}
            labelStyle={_Styles.toggles} />

          <br />

          <SelectableList
            valueLink={{
              value: chartType,
              requestChange: (event, chartType) =>
                  this._onChartTypeChange(event, chartType)
            }}
            style={_Styles.inputs} >
            {LIST_ITEMS.map((item) => {
              return ([
                <ListItem
                  primaryText={item.text}
                  leftIcon={item.icon}
                  value={item.payload} />,
                <Divider />
                  ]);
                })}
          </SelectableList>
        </div>

        <div style={_Styles.divider.root}>
          <div style={_Styles.divider.inner}>
          </div>
        </div>

        <div style={_Styles.previewPanel}>
          {(shouldRenderPreview && valueRange) ? (
            [
              <ASChart
                ref="generatedChart"
                valueRange={valueRange}
                sheetId={SheetStore.getCurrentSheetId()}
                chartContext={this._generateContext()}
                chartStyle={{width: "100%", height: "100%"}}
                showLegend={showLegend}
                redraw={true} />
            ]
          ) : [
            <h2>Incorrect chart configuration.</h2>
          ]}
        </div>

      </Dialog>
    );
  }
}

ASChartDialog.childContextTypes = {
  muiTheme: React.PropTypes.object
};

export default ASChartDialog;
