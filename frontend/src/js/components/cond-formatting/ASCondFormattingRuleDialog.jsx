/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  FormatMapConstructor
} from '../../types/CondFormat';

import React from 'react';

import {Dialog, FlatButton, TextField} from 'material-ui';

import RuleForm from './RuleForm.jsx';

import ASCondFormatRule from '../../classes/ASCondFormatRule';
import ASRange from '../../classes/ASRange';

type RuleDialogProps = {
  id: ?string;
  open: boolean;
  onRequestClose: Callback;
  onSubmitRule: Callback<ASCondFormatRule>;
  rangeValueLink: ReactLink;
  formatterValueLink: ReactLink;
};

function makeASRuleFromObj({ id, rangeStr, formatter }: {
  id: ?string;
  rangeStr: string;
  formatter: FormatMapConstructor;
}): ASCondFormatRule {
  return ASCondFormatRule.fromClasses({ // make a cond format rule id
    condFormatRuleId: id,
    cellLocs: [ASRange.fromExcelString(rangeStr)],
    formatMapConstructor: formatter
  });
}

function submitRuleWithProps({
  id,
  rangeValueLink,
  formatterValueLink,
  onSubmitRule,
  onRequestClose
}: RuleDialogProps) {
  const {value: rangeStr} = rangeValueLink;
  const {value: formatter} = formatterValueLink;

  const rule = makeASRuleFromObj({id, rangeStr, formatter});
  onSubmitRule(rule);
  onRequestClose();
}

export default function ASCondFormattingRuleDialog(props: RuleDialogProps): React.Element {
  const {open, onRequestClose, rangeValueLink, formatterValueLink} = props;

  const cancelAction = <FlatButton
    label="Cancel"
    onTouchTap={onRequestClose}
  />;

  const submitAction = <FlatButton
    label="Submit"
    onTouchTap={(e) => submitRuleWithProps(props)}
  />;

  return (
    <Dialog
      actions={[cancelAction, submitAction]}
      open={open}
      onRequestClose={onRequestClose}
    >
      <TextField valueLink={rangeValueLink} />
      <RuleForm valueLink={formatterValueLink} />
    </Dialog>
  );
}
