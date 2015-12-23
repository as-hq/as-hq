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
import {Dialog, TextField, DropDownMenu} from 'material-ui';

import {Just, Nothing} from '../../AS/Maybe';

import ASColorPicker from '../basic-controls/ASColorPicker.jsx';

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
  initialRule: ?CondFormatRule;
  open: boolean;
  onRequestClose: Callback;
  onSubmitRule: Callback<CondFormatRule>;
};

type DialogCondFormatRule = {
  range: NakedRange;
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
  let def = ({ expr1: '', expr2: '' });
  switch (ruleCondition.tag) {
    case 'OneExpressionCondition':
      let [serverType, {expression}] = ruleCondition.contents;
      switch (serverType) {
        case 'GreaterThan':
          return ({ ...def, conditionType: 'greater_than', expr1: expression });
        case 'LessThan':
          return ({ ...def, conditionType: 'less_than', expr1: expression });
        default:
          throw new Error('Unimplemented');
      }
    case 'TwoExpressionsCondition':
      let [st, xp1, xp2] = ruleCondition.contents;
      let {expression: exp1} = xp1;
      let {expression: exp2} = xp2;
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
    let sel = SelectionStore.getActiveSelection();
    if (!sel) {
      throw new Error('No selection in store');
    }

    return ({
      range: sel.range,
      conditionType: 'greater_than',
      expr1: '',
      expr2: '',
      style: 'bold',
      styleColor: DEFAULT_COLOR
    });
  } else {
    return ({
      range: rule.cellLocs[0].range,
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

}

function convertToServer(rule: DialogCondFormatRule): CondFormatRule {
  return ({
    tag: 'CondFormatRule',
    condFormat: convertStyleToServer(rule),
    condition: convertConditionToServer(rule),
    cellLocs: [{
      tag: 'range',
      sheetId: SheetStateStore.getCurrentSheet().sheetId , // TODO: get current sheet id
      range: rule.range
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

export class ASCondFormattingDialog
  extends React.Component<{}, RuleDialogProps, RuleDialogState>
{
  constructor(props: RuleDialogProps) {
    super(props);

    this.state = {
      rule: convertToClient(props.initialRule)
    };
  }

  linkStateLens<T>(lens: Lens<RuleDialogState, T>): ReactValueLink {
    let self = this;
    return ({
      value: lens.get(self.state),
      requestChange(newValue: T) {
        lens.set(self.state, newValue);
      }
    });
  }

  linkRuleRangeState(): ReactValueLink {
    let self = this;

    return this.linkStateLens({
      get: (state: RuleDialogState) => TC.rangeToExcel(state.rule.range),
      set: (state: RuleDialogState, val: string) => {
        self.setState({ rule: { ...self.state.rule,
          range: TC.excelToRange(val)
        }});
      }
    });
  }

  // TextField, DropDownMenu both support this
  linkRuleState(varName: $Keys<DialogCondFormatRule>): ReactValueLink {
    let self = this;

    return this.linkStateLens({
      get: (state: RuleDialogState) => state.rule[varName],
      set: (state: RuleDialogState, val: any) => {
        self.setState({ rule: { ...self.state.rule, [varName]: val } });
      }
    });
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
          { text: 'Submit', onTouchTap: this._onClickSubmit, ref: 'submit' }
        ]}
        title="Edit conditional formatting rule"
        open={open}
        onRequestClose={onRequestClose}>
        <TextField
          style={standardStyling}
          hintText="Range"
          valueLink={this.linkRuleRangeState()} />
        <br />
        <DropDownMenu
          style={standardStyling}
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
        <DropDownMenu
          style={standardStyling}
          menuItems={STYLING_MENU_ITEMS}
          valueLink={this.linkRuleState('style')} />
        <br />
        {showStyleColorField(this.state.rule) ?
          <ASColorPicker valueLink={this.linkRuleState('styleColor')} /> : null}
        {_.range(7).map(() => <br />)}
      </Dialog>
    );
<<<<<<< HEAD
  }
=======
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

  // TODO: add support for all the different types of conditional formats. For
  // now, this only shows custom stuff.
  _getConditionFromForm(): CondFormatCondition {
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
      tag: 'CustomExpressionCondition',
        contents: {
        expression: this.refs.conditionField.getValue(),
        language: language
      }
    };
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
      condition: this._getConditionFromForm(),
      condFormat: this._getCellPropFromForm()
    };
  },
>>>>>>> condformatNew

  _onClickSubmit() {
    this.props.onSubmitRule(convertToServer(this.state.rule));
    this.props.onRequestClose();
  }
};
