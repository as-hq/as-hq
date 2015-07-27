import React, {PropTypes} from 'react';
import ASSpreadsheet from './ASSpreadsheet.jsx';
import {AppCanvas, RaisedButton, Styles} from 'material-ui';

const ThemeManager = new Styles.ThemeManager();

export default React.createClass({
  propTypes: {
    tasks: PropTypes.array.isRequired,
    onAddTask: PropTypes.func.isRequired,
    onClear: PropTypes.func.isRequired
  },

  getDefaultProps() {
    return {
      tasks: []
    }
  },

  childContextTypes: {
    muiTheme: React.PropTypes.object
  },

  getChildContext() {
    return {
      muiTheme: ThemeManager.getCurrentTheme()
    };
  },

  render() {
    let {onAddTask, onClear, tasks} = this.props;
    return (
      <ASSpreadsheet behavior="default" />
    );
  }
});
