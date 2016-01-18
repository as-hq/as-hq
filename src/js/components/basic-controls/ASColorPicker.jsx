/* @flow */

import React, {PropTypes} from 'react';

export default React.createClass({
  getValue(): string {
    return this.refs.colorPicker.value;
  },

  render(): ReactElement {
    let {value, defaultValue, valueLink, ...etc} = this.props;

    return this.props.valueLink ? (
      <input
        type="color"
        valueLink={valueLink}
        {...etc} />
    ) : (
      <input
        ref="colorPicker"
        type="color"
        value={value}
        defaultValue={defaultValue}
        onChange={this._handleChange}
        {...etc} />
    );
  },

  _handleChange(evt: SyntheticUIEvent) {
    if (evt.target !== null && evt.target !== undefined) {
      let input: any = evt.target;
      this.props.onValueChange(input.value);
    }
  }
});
