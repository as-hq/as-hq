/* @flow */

import type {
  ASCellProp
} from '../../types/Eval';

import type {
  BoolCondition
} from '../../types/CondFormat';

import React from 'react';

import Util from '../../AS/Util';

import ASCondFormatRule from '../../classes/ASCondFormatRule';
import ASRange from '../../classes/ASRange';

import _Styles from '../../styles/cond-formatting/ASCondFormattingDialog';

import ASButton from '../basic-controls/ASButton.jsx';

type ASCondFormattingDialogContentsProps = {
  rules: Array<ASCondFormatRule>;
  onEditRule: (ruleId: string) => void;
  onDeleteRule: (ruleId: string) => void;
};

function conditionDescriptor(condition: BoolCondition): string {
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

function showRule(rule: ASCondFormatRule): string {
  // #needsrefactor why the hell do I have to do .map((r) => Util.rangeToExcel(r)) instead of
  // .map(Util.rangeToExcel) ???
  let rngsStr = Util.String.toSentence(
    rule.cellLocs.map((r) => r.toExcel().toString())
  );

  const {formatMapConstructor} = rule;

  switch (formatMapConstructor.tag) {
    case 'BoolFormatMapConstructor':
      const {
        boolFormatMapCondition,
        boolFormatMapProps: [boolFormatMapProp]
      } = formatMapConstructor;
      return `${rngsStr}: if cell ${
        conditionDescriptor(boolFormatMapCondition)
      }, ${
        styleDescriptor(boolFormatMapProp)
      }`;
    case 'LambdaFormatMapConstructor':
      const {contents} = formatMapConstructor;
      return `${rngsStr}: ${contents}`;
    default:
      throw new Error('Unhandled case!');
  }
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
            onMouseUp={() => onEditRule(rule.condFormatRuleId)}
            label="Edit"
            selectable={false} />
          <ASButton
            style={_Styles.buttons}
            primary={true}
            onMouseUp={() => onDeleteRule(rule.condFormatRuleId)}
            label="Delete"
            selectable={false} />
        </div>
      )}
    </span>
  );
}
