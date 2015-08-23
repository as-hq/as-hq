import React, {PropTypes} from 'react';
import {AppCanvas, Styles} from 'material-ui';
import MenuItem from 'material-ui/lib/menus/menu-item';
import MenuDivider from 'material-ui/lib/menus/menu-divider';

import ASIconMenu from './ASIconMenu.jsx';
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
          minWidth: width,
          width: width,
          height: height
        }}
        onMouseEnter={this._onMouseEnter}
        onMouseLeave={this._onMouseLeave}
      />
    );

    return (
      <ASIconMenu
        ref={'dropdown'}
        iconButtonElement={button}
        desktop={true}
        openDirection={'bottom-right'}
        beforeTouchTap={this._onTouchTap}
        menuStyle={{
          top: height
        }}
      >
        {menuItems.map((itemTitle) =>
          <MenuItem primaryText={itemTitle} />
        )}
      </ASIconMenu>
    );
  },

  _onMouseEnter() {
    this.setState({ hover: true });
  },

  _onMouseLeave() {
    this.setState({ hover: false });
  },

  _onTouchTap() {
    let dd = this.refs.dropdown;
    console.log(dd);
    if (dd.state.open()) {
      dd.close();
    }
  }
});
