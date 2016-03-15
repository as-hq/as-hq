/* @flow */

import React from 'react';

import ControlledCodeField from '../basic-controls/ASControlledCodeField.jsx';

export default function FCCodeField({defaultValue}: {
  defaultValue: ?string;
} = {}): ReactClass {
  let ret = React.createClass({
    render(): React.Element {
      const {valueLink, style} = this.props;
      const mergedStyle = {
        width: '100%',
        height: '100px',
      };

      return (
        <div style={mergedStyle}>
          <ControlledCodeField
            name="cond_format_lambda_editor"
            language="Python"
            style={{height: '100%'}}
            text={valueLink} />
        </div>
      );
    },
  });

  ret.defaultValue = () => (defaultValue || '');

  return ret;
}
