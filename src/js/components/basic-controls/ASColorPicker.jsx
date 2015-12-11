/* @flow */

import React, {PropTypes} from 'react';

export default React.createClass({
  getValue(): string {
    return this.refs.colorPicker.value;
  },

  render(): ReactElement {
    return (
      <input ref="colorPicker" type="color" defaultValue={this.props.defaultValue} />
    );
  }
});
