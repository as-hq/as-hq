/* @flow */

import type {
  Maybe
} from '../../AS/Maybe';

import type {
  Callback,
  Lens
} from '../../types/Base';

import type {
  NakedRange,
  ASRange,
  ASExpression,
  ASCellProp
} from '../../types/Eval';

import type {
  CondFormatRule,
  CondFormatCondition
} from '../../types/Messages';

import type {
  MenuItemRequest
} from 'material-ui';

import _ from 'lodash';

import React, {PropTypes} from 'react';
import {Dialog, TextField} from 'material-ui';

import {Just, Nothing} from '../../AS/Maybe';

import ASColorPicker from '../basic-controls/ASColorPicker.jsx';
import ASDropdownMenu from '../basic-controls/ASDropdownMenu.jsx';

import API from '../../actions/ASApiActionCreators';
import CFStore from '../../stores/ASCondFormatStore';
import SelectionStore from '../../stores/ASSelectionStore';
import SheetStateStore from '../../stores/ASSheetStateStore';

import U from '../../AS/Util';
let {
  Conversion: TC
} = U;

type ConditionMenuItem = 'greater_than'
  | 'less_than'
  | 'between'
  | 'satisfies_python'
  | 'satisfies_excel';

type StyleMenuItem = 'bold'
  | 'italic'
  | 'underline'
  | 'bg_color'
  | 'text_color';

type RuleDialogProps = {
  initialRule?: CondFormatRule;
  variantRange?: boolean;
  open: boolean;
  onRequestClose: Callback;
  onSubmitRule: Callback<CondFormatRule>;
};

type DialogCondFormatRule = {
  id: string; 
  range: string;
  conditionType: ConditionMenuItem;
  expr1: string;
  expr2: string;
  style: StyleMenuItem;
  styleColor: string;
};

type RuleDialogState = {
  rule: DialogCondFormatRule;
};

const CONDITION_MENU_ITEMS = [
  { payload: 'greater_than', text: 'Cell is greater than' },
  { payload: 'less_than', text: 'Cell is less than' },
  { payload: 'between', text: 'Cell is between' },
  { payload: 'satisfies_python', text: 'Cell satisfies Python expression' },
  { payload: 'satisfies_excel', text: 'Cell satisfies Excel expression' }
];

const STYLING_MENU_ITEMS = [
  { payload: 'bold', text: 'Bold' },
  { payload: 'italic', text: 'Italic' },
  { payload: 'underline', text: 'Underline' },
  { payload: 'bg_color', text: 'Background color' },
  { payload: 'text_color', text: 'Text color' }
]

const DEFAULT_COLOR = '#000000';

function convertConditionToClient(ruleCondition: CondFormatCondition): ({
  conditionType: ConditionMenuItem;
  expr1: string;
  expr2: string;
}) {
  let ret = { expr1: '', expr2: '' };
  switch (ruleCondition.tag) {
    case 'GreaterThanCondition':
      return ({
        ...ret,
        conditionType: 'greater_than',
        expr1: ruleCondition.contents.expression
      });
    case 'LessThanCondition':
      return ({
        ...ret,
        conditionType: 'less_than',
        expr1: ruleCondition.contents.expression
      });
    case 'IsBetweenCondition':
      return ({
        conditionType: 'between',
        expr1: ruleCondition.contents[0].expression,
        expr2: ruleCondition.contents[1].expression
      });
    case 'CustomCondition':
      switch (ruleCondition.contents.language) {
        case 'Python':
          return ({
            ...ret,
            conditionType: 'satisfies_python',
            expr1: ruleCondition.contents.expression
          });
        case 'Excel':
          return ({
            ...ret,
            conditionType: 'satisfies_excel',
            expr1: ruleCondition.contents.expression
          });
        default:
          throw new Error('Unsupported language in conditional formatting');
      }
    default:
      throw new Error('Unsupported language in conditional formatting');
  }
}

function convertStyleToClient(ruleStyle: ASCellProp): ({
  style: StyleMenuItem;
  styleColor: string;
}) {
  switch (ruleStyle.tag) {
    case 'Bold':
      return ({ style: 'bold', styleColor: DEFAULT_COLOR });
    case 'Italic':
      return ({ style: 'italic', styleColor: DEFAULT_COLOR });
    case 'Underline':
      return ({ style: 'underline', styleColor: DEFAULT_COLOR });
    case 'TextColor':
      return ({ style: 'text_color', styleColor: ruleStyle.contents });
    case 'FillColor':
      return ({ style: 'bg_color', styleColor: ruleStyle.contents });
    default:
      throw new Error('Unknown boolean cell style prop');
  }
}

function convertToClient(rule: ?CondFormatRule): DialogCondFormatRule {
  if (rule === null || rule === undefined) { // Default
    return ({
      id: "CFRID" + U.Render.getUniqueId(),
      range: '',
      conditionType: 'greater_than',
      expr1: '',
      expr2: '',
      style: 'bold',
      styleColor: DEFAULT_COLOR
    });
  } else {
    return ({
      id: rule.condFormatRuleId,
      range: TC.rangeToExcel(rule.cellLocs[0].range),
      ...convertConditionToClient(rule.condition),
      ...convertStyleToClient(rule.condFormat)
    });
  }
}

function convertStyleToServer(rule: DialogCondFormatRule): ASCellProp {
  switch (rule.style) {
    case 'bold':
      return ({ tag: 'Bold', contents: [] });
    case 'italic':
      return ({ tag: 'Italic', contents: [] });
    case 'underline':
      return ({ tag: 'Underline', contents: [] });
    case 'text_color':
      return ({ tag: 'TextColor', contents: rule.styleColor });
    case 'bg_color':
      return ({ tag: 'FillColor', contents: rule.styleColor });
    default:
      throw new Error('Condition nonexistent');
  }
}

function convertConditionToServer(rule: DialogCondFormatRule): CondFormatCondition {
  switch (rule.conditionType) {
    case 'satisfies_python':
    case 'satisfies_excel':
      return ({
        tag: 'CustomCondition',
        contents: {
          expression: rule.expr1,
          language: rule.conditionType === 'satisfies_python' ? 'Python' : 'Excel'
        }
      });
    case 'greater_than':
      return ({
        tag: 'GreaterThanCondition',
        contents: {
          expression: rule.expr1,
          language: 'Excel'
        }
      })
    case 'less_than':
      return ({
        tag: 'LessThanCondition',
        contents: {
          expression: rule.expr1,
          language: 'Excel'
        }
      })
    case 'between':
      return ({
        tag: 'IsBetweenCondition',
        contents: [
          { expression: rule.expr1, language: 'Excel' },
          { expression: rule.expr2, language: 'Excel' }
        ]
      })
    default:
      throw new Error('Unsupported condition');
  }
}

function convertToServer(rule: DialogCondFormatRule): CondFormatRule {
  return ({
    tag: 'CondFormatRule',
    condFormatRuleId: rule.id, 
    condFormat: convertStyleToServer(rule),
    condition: convertConditionToServer(rule),
    cellLocs: [{
      tag: 'range',
      sheetId: SheetStateStore.getCurrentSheet().sheetId , // TODO: get current sheet id
      range: TC.excelToRange(rule.range)
    }]
  });
}

function shownTextFieldCount(rule: DialogCondFormatRule): number {
  switch (rule.conditionType) {
    case 'satisfies_excel':
    case 'satisfies_python':
    case 'greater_than':
    case 'less_than':
      return 1;
    case 'between':
      return 2;
    default:
      return 0;
  }
}

function showStyleColorField(rule: DialogCondFormatRule): boolean {
  switch (rule.style) {
    case 'bg_color':
    case 'text_color':
      return true;
    default:
      return false;
  }
}

export default class ASCondFormattingRuleDialog
  extends React.Component<{}, RuleDialogProps, RuleDialogState>
{
  constructor(props: RuleDialogProps) {
    super(props);

    this.state = {
      rule: convertToClient(props.initialRule)
    };
  }

  componentDidMount() {
    SelectionStore.addChangeListener(this._onChangeDefaultSelection.bind(this));
  }

  componentWillUnmount() {
    SelectionStore.removeChangeListener(this._onChangeDefaultSelection.bind(this));
  }

  _onChangeDefaultSelection() {
    let sel = SelectionStore.getActiveSelection();

    if (sel && this.props.variantRange) {
      let self = this;
      let {range} = sel;

      this.setState({
        rule: { ...self.state.rule, range: TC.rangeToExcel(range) }
      });
    }
  }

  linkStateLens<T>(lens: Lens<RuleDialogState, T>): ReactLink<T> {
    let self = this;
    return ({
      value: lens.get(self.state),
      requestChange(newValue: T) {
        lens.set(self.state, newValue);
      }
    });
  }

  // TextField, DropDownMenu both support this
  linkRuleState(varName: $Keys<DialogCondFormatRule>): ReactLink {
    let self = this;

    return this.linkStateLens({
      get: (state: RuleDialogState) => state.rule[varName],
      set: (state: RuleDialogState, val: any) => {
        self.setState({ rule: { ...self.state.rule, [varName]: val } });
      }
    });
  }

  clearState() {
    this.setState({ rule: convertToClient() });
  }

  render() {
    let {initialRule, open, onRequestClose} = this.props;

    let standardStyling = {
      width: '400px',
      paddingLeft: '24px'
    };

    return (
      <Dialog
        ref="mainDialog"
        actions={[
          { text: 'Cancel' },
          { text: 'Submit', onTouchTap: this._onClickSubmit.bind(this), ref: 'submit' }
        ]}
        title="Edit conditional formatting rule"
        open={open}
        onRequestClose={onRequestClose}>
        <TextField
          style={standardStyling}
          hintText="Range"
          valueLink={this.linkRuleState('range')} />
        <br />
        <ASDropdownMenu
          menuItems={CONDITION_MENU_ITEMS}
          valueLink={this.linkRuleState('conditionType')} />
        <br />
        {shownTextFieldCount(this.state.rule) >= 1 ? (
          [
            <TextField
              style={standardStyling}
              hintText="Value or formula"
              valueLink={this.linkRuleState('expr1')} />,
            <br />
          ]
        ) : null}
        {shownTextFieldCount(this.state.rule) >= 2 ? (
          [
            <TextField
              style={standardStyling}
              hintText="Value or formula"
              valueLink={this.linkRuleState('expr2')} />,
            <br />
          ]
        ) : null}
        <ASDropdownMenu
          menuItems={STYLING_MENU_ITEMS}
          valueLink={this.linkRuleState('style')} />
        {
          showStyleColorField(this.state.rule)
            ? [
              <br />,
              <ASColorPicker valueLink={this.linkRuleState('styleColor')} />
            ]
            : null
        }
        <br />
        {_.range(7).map(() => <br />)}
      </Dialog>
    );
  }

  _onClickSubmit() {
    this.clearState();
    this.props.onSubmitRule(convertToServer(this.state.rule));
    this.props.onRequestClose();
  }
}
