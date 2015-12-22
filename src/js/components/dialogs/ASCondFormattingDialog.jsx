/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  CondFormatRule
} from '../../types/Messages';

import React from 'react';

import {Dialog, FlatButton} from 'material-ui';

import ASButton from '../basic-controls/ASButton.jsx';
import RuleDialog from './ASCondFormattingRuleDialog.jsx';
import Contents from './ASCondFormattingDialogContents.jsx';

import API from '../../actions/ASApiActionCreators';
import CFStore from '../../stores/ASCondFormatStore';

import Util from '../../AS/Util';

import _ from 'lodash';

export default React.createClass({
  componentDidMount() {
    CFStore.addChangeListener(this._onRulesChange);
  },

  componentWillUnmount() {
    CFStore.removeChangeListener(this._onRulesChange);
  },

  getInitialState() {
    return {
      rules: ([]: Array<CondFormatRule>),
      openRule: -1
    };
  },

  render(): ReactElement {
    let {open, onRequestClose} = this.props;
    let {rules, openRule} = this.state;

    let newRuleAction =
      <FlatButton
        label="New rule"
        secondary={true}
        onTouchTap={this._onCreateRule} />;

    return (
      <div>
        <Dialog
          open={open}
          onRequestClose={onRequestClose}
          title="Conditional formatting"
          actions={[newRuleAction]} >
          <Contents
            rules={rules}
            onEditRule={this._onEditRule}
            onDeleteRule={this._onDeleteRule} />
        </Dialog>
        <RuleDialog
          onSubmitRule={this._onSubmitRule(-2)}
          open={openRule === -2}
          onRequestClose={this._onCloseRule(-2)}/>
        {rules.map((rule, idx) =>
          <RuleDialog
            initialRule={rule}
            onSubmitRule={this._onSubmitRule(idx)}
            open={openRule === idx}
            onRequestClose={this._onCloseRule(idx)}
          />
        )}
      </div>
    );
  },

  _onCreateRule() {
    this.setState({ openRule: -2 });
  },

  _onEditRule(ruleIdx: number): Callback {
    return () => {
      this.setState({
        openRule: ruleIdx
      });
    };
  },

  _onDeleteRule(ruleIdx: number): Callback {
    return () => {
      let rules = _.cloneDeep(CFStore.getRules());
      rules.splice(ruleIdx, 1);
      API.setCondFormattingRules(rules);
    };
  },

  _onSubmitRule(ruleIdx: number): Callback<CondFormatRule> {
    if (ruleIdx === -2) {
      return (newRule) => {
        this._createRule(newRule);
      };
    } else {
      return (newRule) => {
        this._updateRule(ruleIdx, newRule);
      };
    }
  },

  _onCloseRule(ruleIdx: number): Callback {
    return () => {
      this.setState({
        openRule: -1
      });
    };
  },

  _updateRule(ruleIdx: number, newRule: CondFormatRule) {
    let rules = _.cloneDeep(CFStore.getRules());
    rules[ruleIdx] = newRule;
    API.setCondFormattingRules(rules);
  },

  _createRule(newRule: CondFormatRule) {
    let rules = _.cloneDeep(CFStore.getRules());
    rules.push(newRule);
    API.setCondFormattingRules(rules);
  },

  _onRulesChange() {
    this.setState({
      rules: CFStore.getRules()
    });
  }
});
