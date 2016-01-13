/* @flow */

import React from 'react';

import ASColorPicker from '../basic-controls/ASColorPicker.jsx';

let FCColorPicker = React.createClass({
  render(): React.Element {
    return <ASColorPicker {...this.props} />;
  }
});

FCColorPicker.defaultValue = () => { return '#000000'; };

export default FCColorPicker;
