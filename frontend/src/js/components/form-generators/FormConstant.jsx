/* @flow */

import React from 'react';

export default function FormConstant<T>(cnst: T): ReactClass {
  let ret = React.createClass({
    componentDidMount() {
      this.props.valueLink.requestChange(cnst);
    },

    render(): React.Element {
      return <div />;
    }
  });

  ret.defaultValue = () => cnst;

  return ret;
}
