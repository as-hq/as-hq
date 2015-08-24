import React, {PropTypes} from 'react';
import {AppCanvas, Styles} from 'material-ui';

let {Colors} = Styles;

export default React.createClass({
  propTypes: [React.PropTypes.string.isRequired],

  getDefaultProps() {
    return {
    };
  },

  render() {
    let {label} = this.props;

    return (
      <div
        style={{
          color: '#ffffff',
          textAlign: 'center',
          textTransform: 'uppercase',
          textShadow: '-1px -1px 1px #000000',
          fontWeight: 'normal',
          paddingTop: '3px',
          paddingBottom: '3px',
          margin: '0 auto'
        }}
      >
        {label}
      </div>
    );
  }
});
