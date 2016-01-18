/* @flow */

import type {
  Callback
} from '../types/Base';

import React from 'react';

import {Paper} from 'material-ui';

import _Styles from '../styles/ASConnectionBar';

type ASConnectionBarProps = {
};

export default function ASConnectionBar(props: ASConnectionBarProps): React.Element {
  return (
    <Paper style={_Styles.root}>Trouble connecting to the server. Trying to reconnect...</Paper>
  );
}
