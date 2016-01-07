// TODO: flow

import type {
  Callback
} from '../types/Base';

import React from 'react';

import {Paper} from 'material-ui';
import IconButton from 'material-ui/lib/icon-button';

import _Styles from '../styles/ASBottomBar';

type ASBottomBarProps = {
  toggleErrorPane: Callback;
  toggleOutputPane: Callback;
};

export default function ASBottomBar(props: ASBottomBarProps): React.Element {
  const {toggleErrorPane, toggleOutputPane} = props;

  return (
    <Paper style={_Styles.root}>
      <IconButton
        iconClassName="material-icons"
        tooltip="Errors"
        tooltipPosition="top-right"
        style={_Styles.errorsButton}
        iconStyle={_Styles.errorsIcon}
        onClick={toggleErrorPane} >alert_error_outline</IconButton>
      <IconButton
        iconClassName="material-icons"
        tooltip="Cell output"
        tooltipPosition="top-right"
        style={_Styles.outputButton}
        iconStyle={_Styles.outputIcon}
        onClick={toggleOutputPane} >action_code</IconButton>
    </Paper>
  );
}
