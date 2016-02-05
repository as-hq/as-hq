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

type ASBottomBarProps = {
  toggleBottomPane: Callback<string>;
};

export default function ASBottomBar(props: ASBottomBarProps): React.Element {
  const {toggleBottomPane} = props;
  return (
    <Paper style={styles.root}>
      <IconButton
        style={styles.button}
        onClick={ () => {toggleBottomPane('error')} }
        iconClassName="material-icons"
        tooltip="Errors"
        tooltipPosition="top-right"
        tooltipStyles={styles.tooltip} >
        error_outline
      </IconButton>

      <IconButton
        style={styles.button}
        onClick={ () => toggleBottomPane('cell') }
        iconClassName="material-icons"
        tooltip="Cell output"
        tooltipPosition="top-right"
        tooltipStyles={styles.tooltip}>
        label_outline
      </IconButton>

      <IconButton
        style={styles.button}
        onClick={ () => toggleBottomPane('header') }
        iconClassName="material-icons"
        tooltip="Header output"
        tooltipPosition="top-right"
        tooltipStyles={styles.tooltip}>
        input
      </IconButton>

    </Paper>
  );
}

const styles = {
  root: {
    position: 'relative',
    display: 'block',
    height: '24px',
    background: '#212121'
  },

  button: {
    position: 'relative',
    display: 'inline-block',
    width: '40px',
    top: '50%',
    transform: 'translateY(-50%)' // vertically center
  },

  tooltip: {
    top: 0,
    zIndex: 1000 // to be visible on top of spreadsheet when closed
  }
};
