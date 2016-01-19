/* @flow */

import React from 'react';

export default function FormFMap(
  {f, fInverse}: {
    f: (val: any) => any; // would like to do parametrization but BoundT error
    fInverse: (val: any) => any;
  },
  BaseForm: ReactClass
): ReactClass {
  let ret = React.createClass({
    getValueLink(): ReactLink {
      const {
        valueLink: {
          value: extValue, requestChange: extRequestChange
        }
      } = this.props;

      return ({
        value: fInverse(extValue),
        requestChange(val) {
          extRequestChange(f(val));
        }
      });
    },

    render(): React.Element {
      return <BaseForm valueLink={this.getValueLink()} />;
    }
  });

  ret.defaultValue = () => {
    return f(BaseForm.defaultValue());
  };

  return ret;
}
