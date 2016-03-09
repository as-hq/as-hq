// @flow

import type {
  Callback
} from '../types/Base';

import type {
  StoreLink
} from '../types/React';

import React from 'react';
import U from '../AS/Util';

import {Paper} from 'material-ui';
// $FlowFixMe
import IconButton from 'material-ui/lib/icon-button';
// $FlowFixMe
import FontIcon from 'material-ui/lib/font-icon';
import SheetStateStore from '../stores/ASSheetStateStore';
import ConfigActions from '../actions/ASConfigActionCreators';

import {bottomBar as bottomBarZIndex} from '../styles/zIndex';

type Props = {
  toggleBottomPane: Callback<string>;
};

export default class ASBottomBar extends React.Component<{}, Props, {}> {
  $storeLinks: Array<StoreLink>;

  componentDidMount() {
    U.React.addStoreLinks(this, [
      { store: SheetStateStore },
    ]);
  }

  componentWillUnmount() {
    U.React.removeStoreLinks(this);
  }

  render(): ReactElement {
    return (
      <Paper style={styles.root}>
        <IconButton
          style={styles.button}
          onClick={ () => ConfigActions.toggleBottomPane('errors') }
          iconClassName="material-icons"
          tooltip={`Errors (${U.Browser.metaKeyName()}+Alt+E)`}
          tooltipPosition="top-right"
          tooltipStyles={styles.tooltip} >
          error_outline
        </IconButton>

        <IconButton
          style={styles.button}
          onClick={ () => ConfigActions.toggleBottomPane('cell_output') }
          iconClassName="material-icons"
          tooltip={`Cell output (${U.Browser.metaKeyName()}+Alt+O)`}
          tooltipPosition="top-right"
          tooltipStyles={styles.tooltip}>
          label_outline
        </IconButton>

        <IconButton
          style={styles.button}
          onClick={ () => ConfigActions.toggleBottomPane('header_output') }
          iconClassName="material-icons"
          tooltip={`Header output (${U.Browser.metaKeyName()}+Alt+H)`}
          tooltipPosition="top-right"
          tooltipStyles={styles.tooltip}>
          input
        </IconButton>

        <span style={styles.sheetName}>
          { SheetStateStore.getCurrentSheetName() }
        </span>

      </Paper>
    );
  }
}

// TODO flex this shit
const styles = {
  root: {
    position: 'relative',
    display: 'block',
    height: '24px',
    background: '#212121',
    zIndex: bottomBarZIndex,
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
  },

  sheetName: {
    position: 'absolute',
    right: 10,
    top: 3,
    width: 'auto',
    color: 'white',
    fontWeight: 'bold',
  }
};
