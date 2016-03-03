import React from 'react';

import Menu from './ASUnselectedMenu.jsx';
import MenuItem from 'material-ui/lib/menus/menu-item';
import {Mixins} from 'material-ui';
import {rightClickMenu as zIndex} from '../../styles/zIndex';
import FocusActions from '../../actions/ASFocusActionCreators';

let {ClickAwayable, StylePropable} = Mixins;

const TOUCH_TAP_CLOSE_DELAY = 200;

export default React.createClass({
  mixins: [ClickAwayable, StylePropable],

  contextTypes: {
    muiTheme: React.PropTypes.object
  },

  getInitialState() {
    return {
      x: 0,
      y: 0,
      expanded: false,
      menuItems: []
    };
  },

  componentWillUnmount() {
    if (this._timeout) {
      clearTimeout(this._timeout);
    }
  },

  componentClickAway() {
    if (this.state.expanded) {
      this.close();
    }
  },

  openAt(x, y, menuItems) {
    this.setState({
      x,
      y,
      expanded: true,
      menuItems,
    });
  },

  close() {
    if (this.state.expanded) {
      FocusActions.focus('grid'); // restore focus to grid
      this.setState({expanded: false});
    }
  },

  render() {
    let {x, y, expanded, menuItems} = this.state;
    let {
      menuStyle,
      style,
      ...other
    } = this.props;

    let styles = {
      root: {
        display: 'inline-block',
        zIndex,
        position: 'absolute',
        left: x,
        top: y,
      },

      menu: {
        // TODO(joel) confused why we give this a zIndex since its parent has
        // one
        zIndex,
        top: -12,
        left: -12,
        fontFamily: 'Roboto, sans-serif',
        fontSize: '14px'
      }
    };

    let mergedRootStyles = this.mergeAndPrefix(styles.root, style);
    let mergedMenuStyles = this.mergeStyles(styles.menu, menuStyle);

    let menuChildren =
      menuItems.map(({primaryText, onclick}) =>
          <MenuItem
            style={{
              fontFamily: 'Roboto, sans-serif',
              fontSize: '14px',
              lineHeight: '28px'
            }}
            primaryText={primaryText}
            onTouchTap={onclick}
            />
      );
    let menu = expanded
      ? (
        <Menu
          {...other}
          style={mergedMenuStyles}
          animated={false}
          openDirection="bottom-right"
          onItemTouchTap={this._handleItemTouchTap}>
          {menuChildren}
        </Menu>
      ) : null;

    return (
      <div style={mergedRootStyles}>
        {menu}
      </div>
    );
  },

  // timeout is here because some guy on the internet who was probably more
  // experienced put a timeout here.
  _handleItemTouchTap(e, child) {
    this._timeout = setTimeout(() => {
      this.close();
    }, TOUCH_TAP_CLOSE_DELAY);
  }
});
