/* @flow */

import type {
  Callback
} from '../types/Base';

import type { StoreToken } from 'flux';

import React from 'react';
import {Dialog, FlatButton, SelectField, MenuItem} from 'material-ui';
// $FlowFixMe:
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe:
import LightTheme from 'material-ui/lib/styles/raw-themes/light-raw-theme';

import U from '../AS/Util';

import GridStore from '../stores/ASGridStore';

import APIActions from '../actions/APIActionCreators';

type Props = {
  onRequestClose: Callback;
  open: bool;
};

type State = {
  delay: number;
};

const items = [
  <MenuItem key={1} value={1} primaryText="1 second" />,
  <MenuItem key={2} value={2} primaryText="2 seconds" />,
  <MenuItem key={3} value={4} primaryText="4 seconds" />,
  <MenuItem key={4} value={8} primaryText="8 seconds" />,
  <MenuItem key={5} value={16} primaryText="16 seconds" />,
  <MenuItem key={5} value={32} primaryText="32 seconds" />,
];

class AutoEvalDialog extends React.Component {
  static defaultProps = {};
  props: Props;
  state: State;
  _storeToken: StoreToken;

  constructor(props: Props) {
    super(props);
    this.state = {
      delay: 1
    };
  }

  componentDidMount() {
    this._storeToken = GridStore.addListener(() => this.forceUpdate());
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
    const {origin} = GridStore.getActiveSelection();
    const title = `Auto-evaluator for cell ${origin.toExcel().toString()}`;
    const {delay} = this.state;

    const done = () => {
      APIActions.setAutoEval(origin, delay);
      onRequestClose();
    };

    const dismissAction = <FlatButton
      label="Done"
      onTouchTap={done}
    />;

    return (
      <Dialog title={title}
              actions={[dismissAction]}
              open={open}
              onRequestClose={onRequestClose} >

              <SelectField value={delay}
                           onChange={(e,i,v) => this._onDelayChange(e,i,v)}
                           floatingLabelText="Run every..."
                           floatingLabelTextFixed={true}
                           >
                           {items}
             </SelectField>

      </Dialog>
    );
  }

  _onDelayChange(evt, idx, val) {
    this.setState({delay: val});
  }
}

AutoEvalDialog.childContextTypes = {
  muiTheme: React.PropTypes.object
};

export default AutoEvalDialog;
