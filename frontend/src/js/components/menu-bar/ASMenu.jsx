/* @flow */

import type {
  Callback,
  Dict
} from '../../types/Base';

import type {
  SimpleItemSpec,
  MenuItemSpec,
  FileItemSpec,
  ASMenuProps,
  ASMenuState,
  FileInput
} from './types';

import React from 'react';
import ReactDOM from 'react-dom';

import U from '../../AS/Util';
import _Styles from '../../styles/menu-bar/ASMenuBar';
import {asMenu as zIndex} from '../../styles/zIndex';

import {FlatButton, Styles} from 'material-ui';
// $FlowFixMe
import FontIcon from 'material-ui/lib/font-icon';

// $FlowFixMe
import Menu from 'material-ui/lib/menus/menu';
// $FlowFixMe
import MenuItem from 'material-ui/lib/menus/menu-item';
import {Popover} from 'material-ui';

const CONST_PROPS = {
  anchorOrigin: { horizontal: 'left', vertical: 'bottom' },
  menuProps: { desktop: true }
};

export default class ASMenu extends React.Component {
  static defaultProps = {}; 
  props: ASMenuProps;
  state: ASMenuState;

  _fileInputs: Dict<FileInput>;

  constructor(props: ASMenuProps) {
    super(props);
    this._fileInputs = {};
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
          onMouseEnter={() => this._handleMenuHover()} />

        <Popover
          anchorOrigin={CONST_PROPS.anchorOrigin}
          anchorEl={anchor}
          open={open}
          onRequestClose={() => this._handleMenuRequestClose()}>
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
        const items = menuItem.menuItems.map(item => this._getMenuItem(item));

        // menu's nested items have right arrow icons
        return (
          <MenuItem
            primaryText={menuItem.title}
            menuItems={items}
            rightIcon={
              <FontIcon
                style={{right: 0, marginTop: 0}}
                className="material-icons">
                keyboard_arrow_right
              </FontIcon>
            }/>
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
          zIndex,
          cursor: 'pointer',
        };
        const fileItem: FileItemSpec = menuItem;

        return (
          <div style={{position: 'relative'}}>
            <input
              type='file'
              style={inputStyle}
              ref={elem => this._fileInputs[fileItem.title] = elem}
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
    fileItem.callback(this._fileInputs[fileItem.title].files);
    this._handleMenuRequestClose();
  }

  _handleMenuRequestClose() {
    this.props.onRequestClose();
  }
};
