import React, {PropTypes} from 'react';
import {AppCanvas, DropDownMenu} from 'material-ui';

export default React.createClass({
  componentDidMount() {
  },

  getDefaultProps() {
    return { height: '54px' };
  },

  render() {
    let {width, height, menuItems} = this.props;

    return (
      <DropDownMenu
        menuItems={menuItems}
        underlineStyle={{
          display: 'none'
        }}
        style={{
          height: height
        }}
      />
    );
  },

  _onChange(e, idx, menuItem) {
    this.props.onSelect(menuItem.payload);
  }
});
