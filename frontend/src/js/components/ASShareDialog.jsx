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
import Toggle from 'material-ui/lib/toggle';
// $FlowFixMe:
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe:
import LightTheme from 'material-ui/lib/styles/raw-themes/light-raw-theme';

import U from '../AS/Util';

import SheetStore from '../stores/ASSheetStateStore';

type Props = {
  onRequestClose: Callback;
  open: bool;
};

type State = {
  shareable: bool;
};

class ASShareDialog extends React.Component {
  static defaultProps = {};
  props: Props;
  state: State;
  $storeLinks: Array<StoreLink>;
  _linkField: any;

  constructor(props: Props) {
    super(props);
    this.state = {
      shareable: false
    };
  }

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
    const {shareable} = this.state;
    const url = SheetStore.getSheetLink(shareable);

    return (
      <Dialog title="Share"
              actions={[ {text: "Dismiss"} ]}
              open={open}
              onRequestClose={onRequestClose} >

        <TextField ref={elem => this._linkField = elem}
                   value={url}
                   fullWidth={true}
                   onClick={() => this._onLinkClick()} />

        <Toggle label="Don't require AlphaSheets account for access"
                toggled={shareable}
                onToggle={() => this._onShareToggle()} />

      </Dialog>
    );
  }

  _onLinkClick() {
    this._linkField._getInputNode().select();
  }

  _onShareToggle() {
    const {shareable} = this.state;
    this.setState({shareable: !shareable});
  }
}

ASShareDialog.childContextTypes = {
  muiTheme: React.PropTypes.object
};

export default ASShareDialog;
