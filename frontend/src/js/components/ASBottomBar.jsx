// @flow

import type {
  Callback
} from '../types/Base';

import React from 'react';

import {Paper} from 'material-ui';
// $FlowFixMe
import IconButton from 'material-ui/lib/icon-button';
// $FlowFixMe
import FontIcon from 'material-ui/lib/font-icon';

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
        style={_Styles.button}
        onClick={toggleErrorPane}
        iconClassName="material-icons"
        tooltip="Errors"
        tooltipPosition="top-right"
        tooltipStyles={_Styles.tooltip} >
        error_outline
      </IconButton>
      <IconButton
        style={_Styles.button}
        onClick={toggleOutputPane}
        iconClassName="material-icons"
        tooltip="Cell output"
        tooltipPosition="top-right"
        tooltipStyles={_Styles.tooltip}>
        swap_horiz
      </IconButton>

    </Paper>
  );
}
