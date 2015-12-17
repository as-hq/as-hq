/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  SimpleItemSpec,
  MenuItemSpec,
  ASMenuProps,
  ASMenuState
} from './types';

import React from 'react';
import ReactDOM from 'react-dom';

import U from '../../AS/Util';
import _Styles from '../../styles/menu-bar/ASMenuBar';

import {FlatButton} from 'material-ui';

// $FlowFixMe: don't know how to declare this in Flow yet
import Menu from 'material-ui/lib/menus/menu';
// $FlowFixMe: don't know how to declare this in Flow yet
import MenuItem from 'material-ui/lib/menus/menu-item';
import {Popover} from 'material-ui';

const CONST_PROPS = {
  anchorOrigin: { horizontal: 'left', vertical: 'bottom' },
  menuProps: { desktop: true }
};

export default class ASMenu extends React.Component<{}, ASMenuProps, ASMenuState> {
  constructor(props: ASMenuProps) {
    super(props);

    this.state = {
      anchor: null
    };
  }

  componentWillReceiveProps(nextProps: ASMenuProps) {
    if (! this.props.open && nextProps.open) {
      this._placeAnchor();
    } else if (this.props.open && ! nextProps.open) {
      this._unplaceAnchor();
    }
  }

  render(): React.Element {
    let {title, open} = this.props;
    let {anchor} = this.state;

    return (
      <div style={_Styles.menuRoot} >
        <FlatButton
          label={title}
          style={_Styles.menu}
          onTouchTap={this._handleMenuClick.bind(this)}
          onMouseEnter={this._handleMenuHover.bind(this)} />
        <Popover
          anchorOrigin={CONST_PROPS.anchorOrigin}
          anchorEl={anchor}
          open={open}
          onRequestClose={this._handleMenuRequestClose.bind(this)} >
          <Menu
            style={_Styles.menuDropRoot}
            {...CONST_PROPS.menuProps} >
            {this._getMenuItems()}
          </Menu>
        </Popover>
      </div>
    );
  }

  _getMenuItems(): Array<React.Element> {
    return this.props.menuItems.map(this._getMenuItem.bind(this));
  }

  _getMenuItem(menuItem: MenuItemSpec): React.Element {
    switch (menuItem.tag) {
      case 'NestedMenuSpec':
        return (
          <MenuItem primaryText={menuItem.title}>
            {menuItem.menuItems.map(this._getMenuItem.bind(this))}
          </MenuItem>
        );
      case 'SimpleItemSpec':
        return (
          <MenuItem
            primaryText={menuItem.title}
            onTouchTap={this._handleMenuItemClick(menuItem).bind(this)} />
        );
      default:
        throw new Error('Invalid menu item tag');
    }
  }

  _placeAnchor() {
    this.setState({
      anchor: ReactDOM.findDOMNode(this)
    });
  }

  _unplaceAnchor() {
    this.setState({
      anchor: null
    });
  }

  _handleMenuClick() {
    this.props.onClick();
  }

  _handleMenuHover() {
    this.props.onHover();
  }

  _handleMenuItemClick(menuItem: SimpleItemSpec): Callback {
    return () => {
      menuItem.callback();
      this._handleMenuRequestClose();
    };
  }

  _handleMenuRequestClose() {
    this.props.onRequestClose();
  }
};
