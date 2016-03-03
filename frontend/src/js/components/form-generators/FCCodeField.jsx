/* @flow */

import React from 'react';

import ASCodeField from '../basic-controls/ASCodeField.jsx';

export default function FCCodeField({defaultValue}: {
  defaultValue: ?string;
} = {}): ReactClass {
  let ret = React.createClass({
    render(): React.Element {
      const {valueLink, style} = this.props;
      const mergedStyle = {
        width: '100%',
        height: '100px',
        ...style
      };

      return (
        <ASCodeField
          style={mergedStyle}
          language="python"
          text={valueLink}
        />
      );
    }
  });

  ret.defaultValue = () => (defaultValue || '');

  return ret;
}
