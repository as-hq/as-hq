import React, {PropTypes} from 'react';
import {AppCanvas, DropDownMenu} from 'material-ui';

export default React.createClass({
  componentDidMount() {
  },

  render() {
    let {menuItems} = this.props;

    return (
      <DropDownMenu
        menuItems={menuItems}
        underlineStyle={{
          display: 'none'
        }}
      />
    );
  },

  _onChange(e, idx, menuItem) {
    this.props.onSelect(menuItem.payload);
  }
});
