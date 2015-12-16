/* @flow */

import type {
  Callback,
  Dict
} from '../../types/Base';

import type {
  ASMenuBarProps,
  ASMenuBarState
} from './types';

import React from 'react';
import U from '../../AS/Util';

import ASMenu from './ASMenu.jsx';

import _Styles from '../../styles/menu-bar/ASMenuBar';

export default class ASMenuBar extends React.Component<{}, ASMenuBarProps, ASMenuBarState> {
  constructor(props: ASMenuBarProps) {
    super(props);

    this.state = {
      currentMenuIdx: -1
    };
  }

  render(): React.Element {
    let {menus} = this.props;
    let {currentMenuIdx} = this.state;

    return (
      <div style={_Styles.root}>
        { menus.map((menu, idx) =>
          <ASMenu
            open={currentMenuIdx === idx}
            title={menu.title}
            menuItems={menu.menuItems}
            onClick={this._handleMenuClick(idx).bind(this)}
            onHover={this._handleMenuHover(idx).bind(this)}
            onRequestClose={this._handleMenuRequestClose(idx).bind(this)} />
        )}
      </div>
    );
  }

  //handlers
  _handleMenuClick(idx: number): Callback {
    return () => {
      this.setState({
        currentMenuIdx: idx
      });
    };
  }

  _handleMenuHover(idx: number): Callback {
    return () => {
      if (this.state.currentMenuIdx !== -1) {
        this.setState({
          currentMenuIdx: idx
        });
      }
    };
  }

  _handleMenuRequestClose(idx: number): Callback {
    return () => {
      this.setState({
        currentMenuIdx: -1
      });
    };
  }
}
