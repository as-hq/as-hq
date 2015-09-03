import React, {PropTypes} from 'react';
import {Styles} from 'material-ui';
import API from '../actions/ASApiActionCreators';

let {Colors} = Styles;

export default React.createClass({
  contextTypes: {
    muiTheme: PropTypes.object
  },

  componentDidMount() {

  },

  render() {
    return (
      <Paper>
      </Paper>
    );
  }
});
