/* @flow */

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

import _ from 'lodash';

import React from 'react';
import {TextField, DropDownMenu} from 'material-ui';

import Dialog from './DialogWrapper.jsx';
import RuleDialog from './ASCondFormattingRuleDialog.jsx';

import API from '../../actions/ASApiActionCreators';
import CFStore from '../../stores/ASCondFormatStore';

import Util from '../../AS/Util';
import TC from '../../AS/TypeConversions';

type StyleMenuItem = 'bold' | 'italic' | 'underline' | 'bg_color' | 'text_color';

type MUIMenuItem = {
  text: string;
  payload: string;
};

export default React.createClass({
  getInitialState() {
    return {
      showConditionTextField: true,
      showStyleColorField: false,
      currentConditionMenuItem: 'python_matcher',
      currentStyleMenuItem: 'bold'
    };
  },

  render() {
    let {initialRule, open, onRequestClose} = this.props;
    let {showConditionTextField, showStyleColorField} = this.state;

    //TODO: do initialRule

    let standardPadding = { paddingLeft: '24px' };

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
          style={standardPadding}
          ref="range"
          hintText="Range" />
        <br />
        <DropDownMenu
          ref="condition"
          menuItems={
            [
              { payload: 'python_matcher', text: 'Cell satisfies Python expression' },
              { payload: 'excel_matcher', text: 'Cell satisfies Excel expression' }
            ]
          }
          onChange={this._onChangeCondition} />
        <br />
        {showConditionTextField ? (
          [
            <TextField
              style={standardPadding}
              ref="conditionField"
              hintText="Value or formula"/>,
            <br />
          ]
        ) : null}
        <DropDownMenu
          ref="style"
          menuItems={
            [
              { payload: 'bold', text: 'Bold' },
              { payload: 'italic', text: 'Italic' },
              { payload: 'underline', text: 'Underline' },
              { payload: 'bg_color', text: 'Background color' },
              { payload: 'text_color', text: 'Text color' }
            ]
          }
          onChange={this._onChangeStyle} />
        <br />
        {showStyleColorField ? (
          null // TODO!!!!!
        ) : null}

        {_.range(7).map(() => <br />)}
      </Dialog>
    );
  },

  _onChangeCondition(evt, idx: number, menuItem: MUIMenuItem) {
    let {payload} = menuItem;
    this.setState({
      currentConditionMenuItem: payload,
      showConditionTextField: this._showTextField(payload)
    });
  },

  _onChangeStyle(evt, idx: number, menuItem: MUIMenuItem) {
    let {payload} = menuItem;
    this.setState({
      currentStyleMenuItem: payload,
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

  _getCellLocsFromForm(): Array<ASRange> {
    return [TC.simpleToASRange(Util.excelToRange(this.refs.range.getValue()))];
  },

  _getExpressionFromForm(): ASExpression {
    let language = 'Excel';

    switch (this.state.currentConditionMenuItem) {
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
    switch (this.state.currentStyleMenuItem) {
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
