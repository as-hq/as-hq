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
        iconClassName="muidocs-icon-custom-github"
        tooltip="Errors"
        tooltipPosition="top"
        style={{marginLeft: '-32px', ..._Styles.button}}
        iconStyle={_Styles.icon}
        onClick={toggleErrorPane} />
      <IconButton
        iconClassName="muidocs-icon-custom-github"
        tooltip="Cell output"
        tooltipPosition="top"
        style={{marginLeft: '-28px', ..._Styles.button}}
        iconStyle={_Styles.icon}
        onClick={toggleOutputPane} />
    </Paper>
  );
}
