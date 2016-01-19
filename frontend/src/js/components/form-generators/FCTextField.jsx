/* @flow */

import React from 'react';

import {TextField} from 'material-ui';

let FCTextField = React.createClass({
  render(): React.Element {
    return <TextField {...this.props} />;
  }
});

FCTextField.defaultValue = () => '';

export default FCTextField;
