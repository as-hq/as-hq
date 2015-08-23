import React, {PropTypes} from 'react';
import {AppCanvas, IconMenu, Styles} from 'material-ui';
import MenuItem from 'material-ui/lib/menus/menu-item';
import MenuDivider from 'material-ui/lib/menus/menu-divider';

import ASButton from './ASButton.jsx';

import ReactClickAway from 'react-clickaway';

let {Colors} = Styles;

export default React.createClass({
  mixins: [ReactClickAway],

  propTypes: {
    labelElement: React.PropTypes.element.isRequired,
    labelStyle: React.PropTypes.object,
    width: React.PropTypes.oneOfType([
      React.PropTypes.string,
      React.PropTypes.number
    ]),
    height: React.PropTypes.oneOfType([
      React.PropTypes.string,
      React.PropTypes.number
    ]),
    menuItems: React.PropTypes.arrayOf(React.PropTypes.string).isRequired
  },

  componentDidMount() {
  },

  getDefaultProps() {
    return {
      labelStyle: {}
    };
  },

  getInitialState() {
    return {
      open: false,
      hover: false
    }
  },

  render() {
    let {labelElement, labelStyle, width, height, menuItems} = this.props;

    let button = (
      <ASButton
        labelElement={labelElement}
        labelStyle={labelStyle}
        backgroundColor={(this.state.hover) ? Colors.red900 : Colors.grey800}
        style={{
          width: width,
          height: height
        }}
        onMouseEnter={this._onMouseEnter}
        onMouseLeave={this._onMouseLeave}
      />
    );

    return (
      <IconMenu
        iconButtonElement={button}
        desktop={true}
        openDirection={'bottom-right'}
        menuStyle={{
          top: height
        }}
      >
        {menuItems.map((itemTitle) =>
          <MenuItem primaryText={itemTitle} />
        )}
      </IconMenu>
    );
  },

  _onMouseEnter() {
    this.setState({ hover: true });
  },

  _onMouseLeave() {
    this.setState({ hover: false });
  }
});
