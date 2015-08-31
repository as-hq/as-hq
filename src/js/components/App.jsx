import React, {PropTypes} from 'react';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import {AppCanvas, RaisedButton, Styles, AppBar} from 'material-ui';
import API from '../actions/ASApiActionCreators';

const ThemeManager = new Styles.ThemeManager();

export default React.createClass({

  /* When mounting, send a message to the backend to signify a connection */
  componentDidMount() {
    ThemeManager.setTheme(ThemeManager.types.DARK);
    API.sendInitialMessage();
  },
  getDefaultProps() {
    return {}
  },
  childContextTypes: {
    muiTheme: React.PropTypes.object
  },
  getChildContext() {
    return {
      muiTheme: ThemeManager.getCurrentTheme()
    };
  },

  /**************************************************************************************************************************/
  /* Core render method for the whole app */

  render() {
    return (
      <div className="full">
        <AppBar
          style={{
            backgroundColor: Styles.Colors.grey800
          }}
        />
        <ASEvaluationPane behavior="default" ref="evalPane"/>
      </div>
    );
  }
});
