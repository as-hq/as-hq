/* @flow */

import type {
  Callback
} from '../types/Base';

import type { StoreToken } from 'flux';

import React from 'react';
import {Dialog, FlatButton, TextField} from 'material-ui';
// $FlowFixMe:
import Toggle from 'material-ui/lib/toggle';
// $FlowFixMe:
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe:
import LightTheme from 'material-ui/lib/styles/raw-themes/light-raw-theme';

import U from '../AS/Util';

import WorkbookStore from '../stores/ASWorkbookStore';

type Props = {
  onRequestClose: Callback;
  open: bool;
};

type State = {
  accountRequired: bool;
};

class ASShareDialog extends React.Component {
  static defaultProps = {};
  props: Props;
  state: State;
  _storeToken: StoreToken;
  _linkField: any;

  constructor(props: Props) {
    super(props);
    this.state = {
      accountRequired: true
    };
  }

  componentDidMount() {
    this._storeToken = WorkbookStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._storeToken.remove();
  }

  getChildContext(): any {
    return {
      muiTheme: ThemeManager.getMuiTheme(LightTheme)
    };
  }

  render(): React.Element {
    const {open, onRequestClose} = this.props;
    const {accountRequired} = this.state;
    const url = WorkbookStore.getSheetLink(accountRequired);

    const dismissAction = <FlatButton
      label="Dismiss"
      onTouchTap={onRequestClose}
    />;

    return (
      <Dialog title="Share"
              actions={[dismissAction]}
              open={open}
              onRequestClose={onRequestClose} >

        <TextField ref={elem => this._linkField = elem}
                   value={url}
                   fullWidth={true}
                   onClick={() => this._onLinkClick()} />

        <Toggle label="Require AlphaSheets account for access"
                toggled={accountRequired}
                onToggle={() => this._onShareToggle()} />

      </Dialog>
    );
  }

  _onLinkClick() {
    this._linkField._getInputNode().select();
  }

  _onShareToggle() {
    const {accountRequired} = this.state;
    this.setState({accountRequired: !accountRequired});
  }
}

ASShareDialog.childContextTypes = {
  muiTheme: React.PropTypes.object
};

export default ASShareDialog;
