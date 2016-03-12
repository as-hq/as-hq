/* @flow */

import type {
  Callback
} from '../types/Base';

import type {
  StoreLink
} from '../types/React';

import React from 'react';
import {Dialog, TextField} from 'material-ui';
// $FlowFixMe:
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe:
import LightTheme from 'material-ui/lib/styles/raw-themes/light-raw-theme';

import U from '../AS/Util';

import SheetStore from '../stores/ASSheetStateStore';

type Props = {
  onRequestClose: Callback;
  open: bool;
}

class ASShareDialog extends React.Component {
  static defaultProps = {};
  props: Props;
  state: {};
  $storeLinks: Array<StoreLink>;

  componentDidMount() {
    U.React.addStoreLinks(this, [
      { store: SheetStore }
    ]);
  }

  componentWillUnmount() {
    U.React.removeStoreLinks(this);
  }

  getChildContext(): any {
    return {
      muiTheme: ThemeManager.getMuiTheme(LightTheme)
    };
  }

  render(): React.Element {
    const {open, onRequestClose} = this.props;
    const link = SheetStore.getSheetLink();

    return (
      <Dialog
        title="Share"
        actions={[ {text: "Dismiss"} ]}
        open={open}
        onRequestClose={onRequestClose} >

        <TextField
          value={link}
          fullWidth={true} />

      </Dialog>
    );
  }
}

ASShareDialog.childContextTypes = {
  muiTheme: React.PropTypes.object
};

export default ASShareDialog;
