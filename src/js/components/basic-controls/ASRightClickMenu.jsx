import React from 'react';
import ReactTransitionGroup from 'react-addons-transition-group';

import Menu from 'material-ui/lib/menus/menu';
import MenuItem from 'material-ui/lib/menus/menu-item';
import {Mixins} from 'material-ui';

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
      clickAwayBuffer: false,
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
    if (this.state.clickAwayBuffer) {
      this.setState({ clickAwayBuffer: false });
    } else if (this.state.expanded) {
      this.close();
    }
  },

  openAt(x, y, menuItems) {
    console.log('OPENING MENU');
    this.setState({
      x: x,
      y: y,
      clickAwayBuffer: true,
      expanded: true,
      menuItems: menuItems
    });
  },

  close() {
    if (this.state.expanded) {
      this.setState({ clickAwayBuffer: false, expanded: false });
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
        zIndex: 100,
        position: 'absolute',
        left: x,
        top: y - 100
      },

      menu: {
        zIndex: 100,
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
          animated={true}
          openDirection="bottom-right"
          onItemTouchTap={this._handleItemTouchTap}>
          {menuChildren}
        </Menu>
      ) : null;

    return (
      <div style={mergedRootStyles}>
        <ReactTransitionGroup>
          {menu}
        </ReactTransitionGroup>
      </div>
    );
  },

  _handleItemTouchTap(e, child) {
    this._timeout = setTimeout(() => {
      this.close();
    }, TOUCH_TAP_CLOSE_DELAY);
  }
});
