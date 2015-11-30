/* @flow */

import React from 'react';
import {AppBar, FlatButton, Styles} from 'material-ui';

import ASButton from './basic-controls/ASButton.jsx';

let {Colors} = Styles;

export default React.createClass({
  render(): ReactElement {
    return (
      <AppBar
        style={{
          backgroundColor: Colors.grey900
        }}
        onLeftIconButtonTouchTap={this._onAlphaButtonTap}>
        <FlatButton
          label="HEADER"
          onClick={this.props.onEvalHeaderClick} />
      </AppBar>
    );
  },

  _onAlphaButtonTap() {

  }
});
