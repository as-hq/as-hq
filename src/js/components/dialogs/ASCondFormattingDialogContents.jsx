/* @flow */

import type {
  CondFormatRule
} from '../../types/Messages';

import React from 'react';

import Util from '../../AS/Util';

import _Styles from '../../styles/dialogs/ASCondFormattingDialog';

import ASButton from '../basic-controls/ASButton.jsx';

type ASCondFormattingDialogContentsProps = {
  rules: Array<CondFormatRule>;
  onEditRule: (ruleIdx: number) => () => void;
  onDeleteRule: (ruleIdx: number) => () => void;
};

function showRule(rule: CondFormatRule): string {
  // #needsrefactor why the hell do I have to do .map((r) => Util.rangeToExcel(r)) instead of
  // .map(Util.rangeToExcel) ???
  let rngsStr = Util.String.toSentence(
    rule.cellLocs.
      map(Util.Conversion.asLocationToSimple).
      map((r) => Util.Conversion.rangeToExcel(r))
  );
  let cond    = rule.condition.expression;

  return `${rngsStr}: ${cond}`;
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
            onMouseUp={onEditRule(idx)}
            label="Edit"
            selectable={false} />
          <ASButton
            style={_Styles.buttons}
            primary={true}
            onMouseUp={onDeleteRule(idx)}
            label="Delete"
            selectable={false} />
        </div>
      )}
    </span>
  );
}
