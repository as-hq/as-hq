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
import {TextField, DropDownMenu} from 'material-ui';

import {Just, Nothing} from '../../AS/Maybe';

import Dialog from './DialogWrapper.jsx';
import SelectField from '../basic-controls/ASSelectField.jsx';

import API from '../../actions/ASApiActionCreators';
import CFStore from '../../stores/ASCondFormatStore';

import Util from '../../AS/Util';
import TC from '../../AS/TypeConversions';

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

  getDefaultConditionMenuValue(): number {
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

  getDefaultStyleMenuValue(): number {
    return Just(this.props.initialRule)
      .fmap(({condFormat}) => condFormat)
      .fmap(({tag}) => {
        switch (tag) {
          case 'Bold': return 0;
          case 'Italic': return 1;
          case 'Underline': return 2;
          default: return undefined;
        }
      })
      .out() || 0;
  },

  getDefaultConditionMenuItem(): string {
    return this.conditionMenuItems[this.getDefaultConditionMenuValue()].payload;
  },

  getDefaultStyleMenuItem(): string {
    return this.conditionMenuItems[this.getDefaultStyleMenuValue()].payload;
  },

  getInitialState() {
    return {
      showConditionTextField: this._showTextField(this.getDefaultConditionMenuItem()),
      showStyleColorField: this._showColorField(this.getDefaultStyleMenuItem())
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
          defaultValue={
            Just(initialRule)
              .fmap(({cellLocs}) => cellLocs)
              .fmap(([firstLoc]) => firstLoc)
              .fmap(({range}) => range)
              .fmap(x => Util.rangeToExcel(x))
              .out() || ''
          }
          style={standardStyling}
          hintText="Range" />
        <br />
        <SelectField
          ref="condition"
          defaultValue={this.getDefaultConditionMenuValue()}
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
          defaultValue={this.getDefaultStyleMenuValue()}
          style={standardStyling}
          menuItems={this.stylingMenuItems}
          onChange={this._onChangeStyle} />
        <br />
        {showStyleColorField ? (
          null // TODO!!!!!
        ) : null}

        {_.range(7).map(() => <br />)}
      </Dialog>
    );
  },

  _onChangeCondition(evt: any, idx: number, menuItem: MenuItemRequest) {
    let {payload} = menuItem;
    if (! payload) return;

    this.setState({
      showConditionTextField: this._showTextField(payload)
    });
  },

  _onChangeStyle(evt: any, idx: number, menuItem: MenuItemRequest) {
    let {payload} = menuItem;
    if (! payload) return;

    this.setState({
      showStyleColorField: this._showColorField(payload)
    });
  },

  _showTextField(menuItem: string): boolean {
    switch (menuItem) {
      case 'cell_empty':
      case 'cell_not_empty':
        return false;
      default:
        return true;
    }
  },

  _showColorField(menuItem: string): boolean {
    switch (menuItem) {
      case 'bg_color':
      case 'text_color':
        return true;
      default:
        return false;
    }
  },

  _getConditionMenuItem(): string {
    return this.refs.condition.getPayload();
  },

  _getStyleMenuItem(): string {
    return this.refs.style.getPayload();
  },

  _getCellLocsFromForm(): Array<ASRange> {
    return [TC.simpleToASRange(Util.excelToRange(this.refs.range.getValue()))];
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
      case 'text_color':
        //TODO!!!!!!
        return { tag: 'Bold', contents: [] };
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
