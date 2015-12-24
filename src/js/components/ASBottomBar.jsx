// TODO: flow

import type {
  Callback
} from '../types/Base';

import React from 'react';

import {Paper} from 'material-ui';

import _Styles from '../styles/ASBottomBar';

type ASBottomBarProps = {
  memory: number;
  toggleErrorPane: Callback;
};

export default function ASBottomBar(props: ASBottomBarProps): React.Element {
  const {memory, toggleErrorPane} = props;

  return (
    <Paper style={_Styles.root}>

    </Paper>
  );
}
