/* @flow */

import type {
  ASCellProp
} from '../../types/Eval';

import type {
  CondFormatRule,
  CondFormatCondition
} from '../../types/CondFormat';

import React from 'react';

import Util from '../../AS/Util';

import _Styles from '../../styles/dialogs/ASCondFormattingDialog';

import ASButton from '../basic-controls/ASButton.jsx';

type ASCondFormattingDialogContentsProps = {
  rules: Array<CondFormatRule>;
  onEditRule: (ruleId: string) => () => void;
  onDeleteRule: (ruleId: string) => () => void;
};

function conditionDescriptor(condition: CondFormatCondition): string {
  switch (condition.tag) {
    case 'CustomCondition':
      return `satisfies ${condition.contents.language} expression ${condition.contents.expression}`;
    case 'LessThanCondition':
      return `less than ${condition.contents.expression}`;
    case 'GreaterThanCondition':
      return `greater than ${condition.contents.expression}`;
    case 'IsBetweenCondition':
      return `between ${condition.contents[0].expression} and ${condition.contents[1].expression}`;
    default:
      throw new Error('Unsupported condition');
  }
}

function styleDescriptor(prop: ASCellProp): string {
  switch (prop.tag) {
    case 'Bold':
      return 'bold cell';
    case 'Italic':
      return 'italicize cell';
    case 'Underline':
      return 'underline cell';
    case 'TextColor':
      return `make text ${prop.contents}`;
    case 'FillColor':
      return `make background ${prop.contents}`;
    default:
      throw new Error('Unsupported style');
  }
}

function showRule(rule: CondFormatRule): string {
  // #needsrefactor why the hell do I have to do .map((r) => Util.rangeToExcel(r)) instead of
  // .map(Util.rangeToExcel) ???
  let rngsStr = Util.String.toSentence(
    rule.cellLocs.
      map(Util.Conversion.asLocationToSimple).
      map((r) => Util.Conversion.rangeToExcel(r))
  );

  let {condition, condFormat} = rule;

  return `${rngsStr}: if cell ${
    conditionDescriptor(condition)
  }, ${
    styleDescriptor(condFormat)
  }`;
}

function ASCondFormattingRuleLabel(
  props: ({ ruleTitle: string; })
): React.Element {
  let {ruleTitle} = props;

  return (
    <h4 style={_Styles.title}>
      {ruleTitle}
    </h4>
  );
}

export default function ASCondFormattingDialogContents(
  props: ASCondFormattingDialogContentsProps
): React.Element {
  let {rules, onEditRule, onDeleteRule} = props;

  return (
    <span>
      {rules.map((rule, idx) =>
        <div style={_Styles.ruleContainer(idx)}>
          <ASCondFormattingRuleLabel ruleTitle={showRule(rule)} />
          <ASButton
            style={_Styles.buttons}
            onMouseUp={onEditRule(rule.condFormatRuleId)}
            label="Edit"
            selectable={false} />
          <ASButton
            style={_Styles.buttons}
            primary={true}
            onMouseUp={onDeleteRule(rule.condFormatRuleId)}
            label="Delete"
            selectable={false} />
        </div>
      )}
    </span>
  );
}
