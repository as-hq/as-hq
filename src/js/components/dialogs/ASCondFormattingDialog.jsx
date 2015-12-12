/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  CondFormatRule
} from '../../types/Messages';

import React from 'react';

import Dialog from './DialogWrapper.jsx';
import ASButton from '../basic-controls/ASButton.jsx';
import RuleDialog from './ASCondFormattingRuleDialog.jsx';

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

  _showRule(rule: CondFormatRule): string {
    // #needsrefactor why the hell do I have to do .map((r) => Util.rangeToExcel(r)) instead of
    // .map(Util.rangeToExcel) ???
    let rngsStr = Util.String.toSentence(
      rule.cellLocs.
        map(Util.Conversion.asLocationToSimple).
        map((r) => Util.Conversion.rangeToExcel(r))
    );
        // cond    = rule.condition.expression;

    return rngsStr + ": ";
  },

  render(): ReactElement {
    let {open, onRequestClose} = this.props;
    let {rules, openRule} = this.state;

    return (
      <div>
        <Dialog
          open={open}
          onRequestClose={onRequestClose}
          title="Conditional formatting">
          <ASButton
            label="New rule"
            onMouseUp={this._onCreateRule} />
          {rules.map((rule, idx) =>
            <div>
              <div>{this._showRule(rule)}</div>
              <ASButton
                onMouseUp={this._onEditRule(idx)}
                label="Edit"
                selectable={false} />
              <ASButton
                primary={true}
                onMouseUp={this._onDeleteRule(idx)}
                label="Delete"
                selectable={false} />
            </div>
          )}
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
