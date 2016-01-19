/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  SimpleItemSpec,
  MenuItemSpec,
  FileItemSpec,
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
  _fileInput: HTMLInputElement;

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
          onTouchTap={() => this._handleMenuClick()}
          onMouseEnter={() => this._handleMenuHover()}
        />
        <Popover
          anchorOrigin={CONST_PROPS.anchorOrigin}
          anchorEl={anchor}
          open={open}
          onRequestClose={() => this._handleMenuRequestClose()}
        >
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
    return this.props.menuItems.map(item => this._getMenuItem(item));
  }

  _getMenuItem(menuItem: MenuItemSpec): React.Element {
    switch (menuItem.tag) {
      case 'NestedMenuSpec':
        return (
          <MenuItem primaryText={menuItem.title}>
            {menuItem.menuItems.map(item => this._getMenuItem(item))}
          </MenuItem>
        );
      case 'SimpleItemSpec':
        const simpleItem : SimpleItemSpec = menuItem;
        return (
          <MenuItem
            primaryText={menuItem.title}
            onTouchTap={() => this._handleMenuItemClick(simpleItem)}
          />
        );
      case 'FileItemSpec':
        // clickjack by covering the menu item in this transparent file input
        // TODO(joel): this covers up the underlying menu item, causing it to
        // not highlight on hover
        const inputStyle = {
          opacity: 0,
          position: 'absolute',
          top: 0,
          bottom: 0,
          zIndex: 1,
          cursor: 'pointer',
        };
        const fileItem: FileItemSpec = menuItem;

        return (
          <div style={{position: 'relative'}}>
            <input
              type='file'
              style={inputStyle}
              ref={elem => this._fileInput = elem}
              onChange={() => this._handleFileItemClick(fileItem)}
            />
            <MenuItem
              primaryText={menuItem.title}
              // set desktop here so this renders the same size as the other
              // menu items. not sure exactly how material-ui detects its
              // context and renders differently if it's in a div. anyway, this
              // fixes it.
              desktop
            />
          </div>
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

  _handleMenuItemClick(menuItem: SimpleItemSpec) {
    menuItem.callback();
    this._handleMenuRequestClose();
  }

  _handleFileItemClick(fileItem: FileItemSpec) {
    fileItem.callback(this._fileInput.files);
    this._handleMenuRequestClose();
  }

  _handleMenuRequestClose() {
    this.props.onRequestClose();
  }
};
