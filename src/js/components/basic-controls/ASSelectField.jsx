import React from 'react';
import {SelectField} from 'material-ui';

export default React.createClass({
  getInitialState() {
    return ({
      selectedIndex: this.props.defaultValue,
      selectedPayload:
        this.props.menuItems[
          this.props.defaultValue
        ].payload
    });
  },

  getPayload() {
    return this.state.selectedPayload;
  },

  render() {
    // discard onChange
    let {defaultValue, onChange, ...props} = this.props;
    let {selectedIndex} = this.state;

    return (
      <SelectField
        selectedIndex={selectedIndex}
        onChange={this._onChange}
        {...props}
      />
    );
  },

  _onChange(e, idx, menuItem) {
    this.setState({
      selectedIndex: idx,
      selectedPayload: menuItem.payload
    });
    this.props.onChange(e, idx, menuItem);
  }
});
