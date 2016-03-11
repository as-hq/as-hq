/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  FormatMapConstructor,
  BoolFormatMapConstructor
} from '../../types/CondFormat';

import type {
  StoreLink
} from '../../types/React';

import type { StoreToken } from 'flux';

import React from 'react';

import {Dialog, FlatButton} from 'material-ui';

import ASButton from '../basic-controls/ASButton.jsx';
import RuleDialog from './ASCondFormattingRuleDialog.jsx';
import Contents from './ASCondFormattingDialogContents.jsx';

import API from '../../actions/ASApiActionCreators';
import CFStore from '../../stores/ASCondFormatStore';
import GridStore from '../../stores/ASGridStore';

import Util from '../../AS/Util';

import ASCondFormatRule from '../../classes/ASCondFormatRule';

import _ from 'lodash';

type ASCondFormattingDialogProps = {
  open: boolean;
  onRequestClose: Callback;
};

type ASCondFormattingDialogState = {
  dialogMode: 'closed' | 'create' | 'edit';
  currentId: ?string;
  currentFormatter: ?FormatMapConstructor;
  currentRange: ?string;
};

export default class ASCondFormattingDialog extends React.Component {
  static defaultProps: {} = {}; 
  props: ASCondFormattingDialogProps;
  state: ASCondFormattingDialogState;

  $storeLinks: Array<StoreLink>;
  _selectionListener: StoreToken;

  constructor(props: ASCondFormattingDialogProps) {
    super(props);

    this.state = {
      dialogMode: 'closed',
      currentId: null,
      currentFormatter: null,
      currentRange: null
    };
  }

  componentDidMount() {
    Util.React.addStoreLinks(this, [
      { store: CFStore },
    ]);
    this._selectionListener = GridStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    Util.React.removeStoreLinks(this);
    this._selectionListener.remove();
  }

  componentWillReceiveProps(nextProps: ASCondFormattingDialogProps) {
    if (nextProps.open && !this.props.open) {
      // if this dialog is useless, just jump to new rule
      const rules = this._getRules();
      if (rules.length === 0) { // no current rules, so let's open the dialog
        this._openCreateRuleDialog();
      }
    }
  }

  getValueLink(name: $Keys<ASCondFormattingDialogState>): ReactLink {
    const self = this;

    return ({
      value: self.state[name],
      requestChange(val) {
        self.setState({ ...self.state, [name]: val });
      }
    });
  }

  render(): React.Element {
    const {open, onRequestClose} = this.props;
    const {
      currentId,
      currentFormatter,
      currentRange,
      dialogMode
    } = this.state;

    const rules = this._getRules();

    const newRuleAction =
      <FlatButton
        label="New rule"
        secondary={true}
        onTouchTap={() => this._openCreateRuleDialog()} />;

    const doneAction =
      <FlatButton
        label="Done"
        secondary={true}
        onTouchTap={this.props.onRequestClose} />;

    // NOTE: open prop is there to enable animating in / out
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
          id={currentId}
          onSubmitRule={(rule) => this._onSubmitRule(rule)}
          open={dialogMode !== 'closed'}
          onRequestClose={() => this._onCloseRule()}
          formatterValueLink={this.getValueLink('currentFormatter')}
          rangeValueLink={this.getValueLink('currentRange')}
        />
      </div>
    );
  }

  _getRules(): Array<ASCondFormatRule> {
    const {range} = GridStore.getActiveSelection();
    return CFStore.getRulesApplyingToRange(range);
  }

  _openCreateRuleDialog() {
    const {range} = GridStore.getActiveSelection();
    const defaultRangeStr = range.toExcel().toString()

    const defaultFormatter: BoolFormatMapConstructor = {
      tag: 'BoolFormatMapConstructor',
      // $FlowFixMe: union error
      boolFormatMapCondition: {
        tag: 'GreaterThanCondition',
        contents: {
          tag: 'Expression',
          expression: ''
        }
      },
      boolFormatMapProps: [{
        tag: 'TextColor', contents: '#000000'
      }]
    };

    this.setState({
      dialogMode: "create",
      currentId: null,
      currentFormatter: defaultFormatter,
      currentRange: defaultRangeStr
    });
  }

  _onEditRule(ruleId: string) {
    CFStore.withRuleById(ruleId, (rule) => {
      this.setState({
        currentId: ruleId,
        currentFormatter: rule.formatMapConstructor, // xcxc
        currentRange: rule.cellLocs[0].toExcel().toString(),
        dialogMode: 'edit'
      });
    });
  }

  _onDeleteRule(ruleId: string) {
    API.removeCondFormattingRule(ruleId);
  }

  _onSubmitRule(rule: ASCondFormatRule) {
    const {dialogMode} = this.state;

    if (dialogMode === "create") {
      this._createRule(rule);
    } else {
      this._updateRule(rule);
    }
  }

  _onCloseRule() {
    this.setState({
      dialogMode: "closed"
    });
  }

  _updateRule(newRule: ASCondFormatRule) {
    API.updateCondFormattingRule(newRule);
  }

  _createRule(newRule: ASCondFormatRule) {
    API.updateCondFormattingRule(newRule);
  }
}
