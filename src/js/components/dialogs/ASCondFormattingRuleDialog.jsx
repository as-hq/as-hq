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

export default (React.createClass({
  propTypes: {
    initialRule: PropTypes.any,
    open: PropTypes.bool.isRequired,
    onRequestClose: PropTypes.func.isRequired
  },

  getDefaultProps(): RuleDialogProps {
    return {
      initialRule: null,
      open: false,
      onRequestClose: () => {}
    }
  },

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
          ref="range"
          defaultValue={
            Just(initialRule)
              .fmap(({cellLocs: [firstCellLoc]}) => firstCellLoc)
              .fmap(Util.rangeToExcel)
              .out() || ''
          }
          style={standardPadding}
          hintText="Range" />
        <br />
        <DropDownMenu
          ref="condition"
          selectedIndex={
            Just(initialRule)
              .fmap(({condition}) => condition)
              .fmap(({language}) => language)
              .fmap((language) => {
                switch (language) {
                  case 'Python': return 0;
                  case 'Excel': return 1;
                }
              })
              .out() || 0
          }
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
              ref="conditionField"
              defaultValue={
                Just(initialRule)
                  .fmap(({condition}) => condition)
                  .fmap(({expression}) => expression)
                  .out() || ''
              }
              style={standardPadding}
              hintText="Value or formula"/>,
            <br />
          ]
        ) : null}
        <DropDownMenu
          ref="style"
          selectedIndex={
            Just(initialRule)
              .fmap(({condFormat}) => condFormat)
              .fmap(({tag}) => {
                switch (tag) {
                  case 'Bold': return 0;
                  case 'Italic': return 1;
                  case 'Underline': return 2;
                  default: return undefined;
                }
              })
              .out() || 0
          }
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

  _onChangeCondition(evt: any, idx: number, menuItem: MenuItemRequest) {
    let {payload} = menuItem;
    if (! payload) return;

    this.setState({
      currentConditionMenuItem: payload,
      showConditionTextField: this._showTextField(payload)
    });
  },

  _onChangeStyle(evt: any, idx: number, menuItem: MenuItemRequest) {
    let {payload} = menuItem;
    if (! payload) return;

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
}) : ReactClass<RuleDialogProps, RuleDialogProps, any>);
