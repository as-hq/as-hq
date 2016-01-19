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
    let {menus, style} = this.props;
    let {currentMenuIdx} = this.state;

    return (
      <div style={{..._Styles.root, ...style}}>
        { menus.map((menu, idx) =>
          <ASMenu
            open={currentMenuIdx === idx}
            title={menu.title}
            menuItems={menu.menuItems}
            onClick={() => this._handleMenuClick(idx)}
            onHover={() => this._handleMenuHover(idx)}
            onRequestClose={() => this._handleMenuRequestClose()}
          />
        )}
      </div>
    );
  }

  //handlers
  _handleMenuClick(currentMenuIdx: number) {
    this.setState({ currentMenuIdx });
  }

  _handleMenuHover(currentMenuIdx: number) {
    if (this.state.currentMenuIdx !== -1) {
      this.setState({ currentMenuIdx });
    }
  }

  _handleMenuRequestClose() {
    this.setState({ currentMenuIdx: -1 });
  }
}
