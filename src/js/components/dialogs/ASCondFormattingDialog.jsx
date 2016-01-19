/* @flow */

import type {
  Callback
} from '../../types/Base';

import React from 'react';

import {Dialog, FlatButton} from 'material-ui';

import ASButton from '../basic-controls/ASButton.jsx';
import RuleDialog from './ASCondFormattingRuleDialog.jsx';
import Contents from './ASCondFormattingDialogContents.jsx';

import API from '../../actions/ASApiActionCreators';
import CFStore from '../../stores/ASCondFormatStore';

import Util from '../../AS/Util';

import ASCondFormatRule from '../../classes/ASCondFormatRule';

import _ from 'lodash';

type ASCondFormattingDialogProps = {
  open: boolean;
  onRequestClose: Callback;
};

type ASCondFormattingDialogState = {
  rules: Array<ASCondFormatRule>;
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
    this._boundOnRulesChange = () => this._onRulesChange();
    CFStore.addChangeListener(this._boundOnRulesChange);
  }

  componentWillUnmount() {
    CFStore.removeChangeListener(this._boundOnRulesChange);
  }

  render(): React.Element {
    let {open, onRequestClose} = this.props;
    let {rules, openRule} = this.state;

    let newRuleAction = (
      <FlatButton
        label="New rule"
        secondary={true}
        onTouchTap={() => this._onCreateRule()}
      />
    );

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
            onEditRule={ruleId => this._onEditRule(ruleId)}
            onDeleteRule={ruleId => this._onDeleteRule(ruleId)} />
        </Dialog>
        <RuleDialog
          variantRange={true}
          onSubmitRule={newRule => this._onSubmitRule("create", newRule)}
          open={openRule === "create"}
          onRequestClose={() => this._onCloseRule("create")}/>
        {rules.map((rule) =>
          <RuleDialog
            initialRule={rule}
            onSubmitRule={newRule => this._onSubmitRule(rule.condFormatRuleId, newRule)}
            open={openRule === rule.condFormatRuleId}
            onRequestClose={() => this._onCloseRule(rule.condFormatRuleId)}
          />
        )}
      </div>
    );
  }

  _onCreateRule() {
    this.setState({ openRule: "create" });
  }

  _onEditRule(ruleId: string): Callback {
    this.setState({
      openRule: ruleId
    });
  }

  _onDeleteRule(ruleId: string): Callback {
    API.removeCondFormattingRule(ruleId);
  }

  _onSubmitRule(ruleId: string, newRule: ASCondFormatRule) {
    return ruleId === "create" ?
      this._createRule(newRule) :
      this._updateRule(newRule);
  }

  _onCloseRule(ruleId: string): Callback {
    this.setState({
      openRule: "closed"
    });
  }

  _updateRule(newRule: ASCondFormatRule) {
    API.updateCondFormattingRule(newRule);
  }

  _createRule(newRule: ASCondFormatRule) {
    API.updateCondFormattingRule(newRule);
  }

  _onRulesChange() {
    this.setState({
      rules: CFStore.getRules()
    });
  }
}
