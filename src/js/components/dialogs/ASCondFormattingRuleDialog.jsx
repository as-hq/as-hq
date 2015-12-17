/* @flow */

import type {
  Maybe
} from '../../AS/Maybe';

import type {
  Callback
} from '../../types/Base';

import type {
  ASRange,
  ASExpression,
  ASCellProp
} from '../../types/Eval';

import type {
  CondFormatRule
} from '../../types/Messages';

import type {
  MenuItemRequest
} from 'material-ui';

import _ from 'lodash';

import React, {PropTypes} from 'react';
import {Dialog, TextField, DropDownMenu} from 'material-ui';

import {Just, Nothing} from '../../AS/Maybe';

import SelectField from '../basic-controls/ASSelectField.jsx';
import ASColorPicker from '../basic-controls/ASColorPicker.jsx';

import API from '../../actions/ASApiActionCreators';
import CFStore from '../../stores/ASCondFormatStore';
import SelectionStore from '../../stores/ASSelectionStore';

import U from '../../AS/Util';
let {
  Conversion: TC
} = U;

type StyleMenuItem = 'bold' | 'italic' | 'underline' | 'bg_color' | 'text_color';

type RuleDialogProps = {
  initialRule: ?CondFormatRule;
  open: boolean;
  onRequestClose: () => void;
};

export default React.createClass({
  conditionMenuItems: [
    { payload: 'python_matcher', text: 'Cell satisfies Python expression' },
    { payload: 'excel_matcher', text: 'Cell satisfies Excel expression' }
  ],

  stylingMenuItems: [
    { payload: 'bold', text: 'Bold' },
    { payload: 'italic', text: 'Italic' },
    { payload: 'underline', text: 'Underline' },
    { payload: 'bg_color', text: 'Background color' },
    { payload: 'text_color', text: 'Text color' }
  ],

  getInitialConditionMenuValue(): number {
    return Just(this.props.initialRule)
      .fmap(({condition}) => condition)
      .fmap(({language}) => language)
      .fmap((language) => {
        switch (language) {
          case 'Python': return 0;
          case 'Excel': return 1;
        }
      })
      .out() || 0;
  },

  getInitialConditionMenuPayload(): string {
    return this.conditionMenuItems[this.getInitialConditionMenuValue()].payload;
  },

  getInitialStyleMenuValue(): number {
    return Just(this.props.initialRule)
      .fmap(({condFormat}) => condFormat)
      .fmap(({tag}) => {
        switch (tag) {
          case 'Bold': return 0;
          case 'Italic': return 1;
          case 'Underline': return 2;
          case 'FillColor': return 3;
          case 'TextColor': return 4;
          default: return undefined;
        }
      })
      .out() || 0;
  },

  getInitialStyleMenuPayload(): string {
    return this.stylingMenuItems[this.getInitialStyleMenuValue()].payload;
  },

  getInitialColorPickerColor(): string {
    let initRule = this.props.initialRule;
    if (initRule != null) {
      let format = initRule.condFormat;
      if (format.tag === 'FillColor' || format.tag === 'TextColor') {
        return U.Conversion.colorToHtml(format.contents);
      }
    }
    return "#000000";
  },

  getInitialRange(): string {
    let activeSel = SelectionStore.getActiveSelection(),
        curSelStr = (activeSel != null) ? U.Conversion.rangeToExcel(activeSel.range) : '';

    return Just(this.props.initialRule)
      .fmap(({cellLocs}) => cellLocs)
      .fmap(([firstLoc]) => firstLoc)
      .fmap(({range}) => range)
      .fmap(x => U.Conversion.rangeToExcel(x))
      .out() || curSelStr;
  },

  getInitialState() {
    return {
      showConditionTextField: this._showTextField(this._getConditionMenuItem()),
      showStyleColorField: this._showColorField(this._getStyleMenuItem())
    };
  },

  render() {
    let {initialRule, open, onRequestClose} = this.props;
    let {showConditionTextField, showStyleColorField} = this.state;

    let standardStyling = {
      width: '400px',
      paddingLeft: '24px'
    };

    return (
      <Dialog
        ref="mainDialog"
        actions={[
          { text: 'Cancel' },
          { text: 'Submit', onTouchTap: this._onClickSubmit, ref: 'submit' }
        ]}
        title="Edit conditional formatting rule"
        open={open}
        onRequestClose={onRequestClose}>
        <TextField
          ref="range"
          defaultValue={this.getInitialRange()}
          style={standardStyling}
          hintText="Range" />
        <br />
        <SelectField
          ref="condition"
          defaultValue={this.getInitialConditionMenuValue()}
          style={standardStyling}
          menuItems={this.conditionMenuItems}
          onChange={this._onChangeCondition} />
        <br />
        {showConditionTextField ? (
          [
            <TextField
              ref="conditionField"
              defaultValue={
                Just(initialRule)
                  .fmap(({condition}) => condition)
                  .fmap(({expression}) => expression)
                  .out() || ''
              }
              style={standardStyling}
              hintText="Value or formula"/>,
            <br />
          ]
        ) : null}
        <SelectField
          ref="style"
          defaultValue={this.getInitialStyleMenuValue()}
          style={standardStyling}
          menuItems={this.stylingMenuItems}
          onChange={this._onChangeStyle} />
        <br />
        {showStyleColorField ?
          <ASColorPicker ref="colorPicker"
                         defaultValue={this.getInitialColorPickerColor()} /> : null}
        {_.range(7).map(() => <br />)}
      </Dialog>
    );
  },

  _onChangeCondition(evt: any, idx: number, menuItem: MenuItemRequest) {
    this.setState({
      showConditionTextField: this._showTextField(menuItem.payload)
    });
  },

  _onChangeStyle(evt: any, idx: number, menuItem: MenuItemRequest) {
    this.setState({
      showStyleColorField: this._showColorField(menuItem.payload)
    });
  },

  _showTextField(menuItemText: ?string): boolean {
    switch (menuItemText) {
      case 'cell_empty':
      case 'cell_not_empty':
        return false;
      default:
        return true;
    }
  },

  _showColorField(menuItemText: ?string): boolean {
    switch (menuItemText) {
      case 'bg_color':
      case 'text_color':
        return true;
      default:
        return false;
    }
  },

  _getConditionMenuItem(): string {
    if (this.refs.condition) {
      return this.refs.condition.getPayload();
    } else {
      return this.getInitialConditionMenuPayload();
    }
  },

  _getStyleMenuItem(): string {
    if (this.refs.style) {
      return this.refs.style.getPayload();
    } else {
      return this.getInitialStyleMenuPayload();
    }
  },

  _getCellLocsFromForm(): Array<ASRange> {
    return [U.Conversion.simpleToASRange(U.Conversion.excelToRange(this.refs.range.getValue()))];
  },

  _getExpressionFromForm(): ASExpression {
    let language = 'Excel';

    switch (this._getConditionMenuItem()) {
      case 'python_matcher':
        language = 'Python';
        break;
      default:
        language = 'Excel';
        break;
    }

    return {
      tag: 'Expression',
      expression: this.refs.conditionField.getValue(),
      language: language
    }
  },

  _getCellPropFromForm(): ASCellProp {
    switch (this._getStyleMenuItem()) {
      case 'bold':
        return { tag: 'Bold', contents: [] };
      case 'italic':
        return { tag: 'Italic', contents: [] };
      case 'underline':
        return { tag: 'Underline', contents: [] };
      case 'bg_color':
        return { tag: 'FillColor', contents: this.refs.colorPicker.getValue() };
      case 'text_color':
        return { tag: 'TextColor', contents: this.refs.colorPicker.getValue() };
      default:
        return { tag: 'Bold', contents: [] }; //unreachable
    }
  },

  _getRuleFromForm(): CondFormatRule {
    return {
      tag: 'CondFormatRule',
      cellLocs: this._getCellLocsFromForm(),
      condition: this._getExpressionFromForm(),
      condFormat: this._getCellPropFromForm()
    };
  },

  _onClickSubmit() {
    this.props.onSubmitRule(this._getRuleFromForm());
    this.props.onRequestClose();
  }
});
