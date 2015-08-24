import React, {PropTypes} from 'react';
import {AppCanvas, Styles} from 'material-ui';

let {Colors} = Styles;

export default React.createClass({
  getDefaultProps() {
    return {
      height: '110px'
    };
  },

  render() {
    let {height} = this.props;

    return (
      <div
        style={{
          display: 'inline-block',
          marginLeft: '10px',
          width: '1px',
          height: height,
          verticalAlign: 'top',
          boxShadow: '0px 0px 1px 0px rgba(255, 255, 255, 0.25)',
          backgroundColor: Colors.grey900
        }}
      />
    );
  }
});
