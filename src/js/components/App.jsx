import React, {PropTypes} from 'react';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import ASTabs from './ASTabs.jsx';
import {AppCanvas, RaisedButton, Styles, AppBar} from 'material-ui';

const ThemeManager = new Styles.ThemeManager();

export default React.createClass({
  componentDidMount() {
    ThemeManager.setTheme(ThemeManager.types.DARK);
  },

  getDefaultProps() {
    return {
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
    return (
      <div className="full">
        <AppBar
          style={{
            backgroundColor: Styles.Colors.grey800
          }} />
        <ASTabs />
        <ASEvaluationPane behavior="default" />
      </div>
    );
  }
});
