/* @flow */

import React, {PropTypes} from 'react';

export default React.createClass({
  getValue(): string {
    return this.refs.colorPicker.value;
  },

  render(): ReactElement {
    let {value, defaultValue} = this.props;

    return (
      <input
        ref="colorPicker"
        type="color"
        value={value}
        defaultValue={defaultValue}
        onChange={this._handleChange} />
    );
  },

  _handleChange(evt: SyntheticUIEvent) {
    if (evt.target !== null && evt.target !== undefined) {
      let input: any = evt.target;
      this.props.onValueChange(input.value);
    }
  }
});
