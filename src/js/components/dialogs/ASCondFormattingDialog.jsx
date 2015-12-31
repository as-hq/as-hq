/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  CondFormatRule
} from '../../types/CondFormat';

import React from 'react';

import {Dialog, FlatButton} from 'material-ui';

import ASButton from '../basic-controls/ASButton.jsx';
import RuleDialog from './ASCondFormattingRuleDialog.jsx';
import Contents from './ASCondFormattingDialogContents.jsx';

import API from '../../actions/ASApiActionCreators';
import CFStore from '../../stores/ASCondFormatStore';

import Util from '../../AS/Util';

import _ from 'lodash';

type ASCondFormattingDialogProps = {
  open: boolean;
  onRequestClose: Callback;
};

type ASCondFormattingDialogState = {
  rules: Array<CondFormatRule>;
  openRule: string;
};

export default class ASCondFormattingDialog
  extends React.Component<{}, ASCondFormattingDialogProps, ASCondFormattingDialogState>
{
  constructor(props: ASCondFormattingDialogProps) {
    super(props);

    this.state = {
      rules: [],
      openRule: "closed"
    };
  }

  componentDidMount() {
    CFStore.addChangeListener(this._onRulesChange.bind(this));
  }

  componentWillUnmount() {
    CFStore.removeChangeListener(this._onRulesChange.bind(this));
  }

  render(): React.Element {
    let {open, onRequestClose} = this.props;
    let {rules, openRule} = this.state;

    let newRuleAction =
      <FlatButton
        label="New rule"
        secondary={true}
        onTouchTap={this._onCreateRule.bind(this)} />;

    let doneAction =
      <FlatButton
        label="Done"
        secondary={true}
        onTouchTap={this.props.onRequestClose} />;

    return (
      <div>
        <Dialog
          open={open}
          onRequestClose={onRequestClose}
          title="Conditional formatting"
          actions={[newRuleAction, doneAction]} >
          <Contents
            rules={rules}
            onEditRule={this._onEditRule.bind(this)}
            onDeleteRule={this._onDeleteRule.bind(this)} />
        </Dialog>
        <RuleDialog
          variantRange={true}
          onSubmitRule={this._onSubmitRule("create").bind(this)}
          open={openRule === "create"}
          onRequestClose={this._onCloseRule("create").bind(this)}/>
        {rules.map((rule) =>
          <RuleDialog
            initialRule={rule}
            onSubmitRule={this._onSubmitRule(rule.condFormatRuleId).bind(this)}
            open={openRule === rule.condFormatRuleId}
            onRequestClose={this._onCloseRule(rule.condFormatRuleId).bind(this)}
          />
        )}
      </div>
    );
  }

  _onCreateRule() {
    this.setState({ openRule: "create" });
  }

  _onEditRule(ruleId: string): Callback {
    return () => {
      this.setState({
        openRule: ruleId
      });
    };
  }

  _onDeleteRule(ruleId: string): Callback {
    return () => {
      API.removeCondFormattingRule(ruleId); 
    };
  }

  _onSubmitRule(ruleId: string): Callback<CondFormatRule> {
    if (ruleId === "create") {
      return (newRule) => {
        this._createRule(newRule);
      };
    } else {
      return (newRule) => {
        this._updateRule(newRule);
      };
    }
  }

  _onCloseRule(ruleId: string): Callback {
    return () => {
      this.setState({
        openRule: "closed"
      });
    };
  }

  _updateRule(newRule: CondFormatRule) {
    API.updateCondFormattingRule(newRule); 
  }

  _createRule(newRule: CondFormatRule) {
    API.updateCondFormattingRule(newRule); 
  }

  _onRulesChange() {
    this.setState({
      rules: CFStore.getRules()
    });
  }
}
