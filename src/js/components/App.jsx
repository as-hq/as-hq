import React, {PropTypes} from 'react';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import {AppCanvas, LeftNav, Paper, Styles} from 'material-ui';
import ASNavBar from './ASNavBar.jsx';
import ASRibbon from './ASRibbon.jsx';
import API from '../actions/ASApiActionCreators';

const ThemeManager = new Styles.ThemeManager();

export default React.createClass({

  /* When mounting, send a message to the backend to signify a connection */
  componentDidMount() {
    ThemeManager.setTheme(ThemeManager.types.DARK);
    API.sendInitialMessage();
  },
  getInitialState() {
    return {
      activeDocumentTab: 'test',
      activeRibbonTab: 'Home'
    }
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
    let leftNavMenuItems = [
      { route: 'all-files', text: 'All files' },
      { route: 'logout', text: 'Log out' }
    ];

    return (
      <div className="full">
        <LeftNav
          ref="leftNav"
          menuItems={leftNavMenuItems}
          docked={false}
        />
        <ASNavBar
          onDocumentTabChange={this._onDocumentTabChange}
          onRibbonTabChange={this._onRibbonTabChange}
          onAlphaButtonTap={this._onAlphaButtonTap}
        />
        <ASRibbon activeTab={this.state.activeRibbonTab}/>
        <ASEvaluationPane behavior="default" ref="evalPane"/>
      </div>
    );
  },

/**************************************************************************************************************************/
/* Top-level ui state changes */

  _onDocumentTabChange(tabKey) {
    this.setState({ activeDocumentTab: tabKey });
  },

  _onRibbonTabChange(tabTitle) {
    this.setState({ activeRibbonTab: tabTitle });
  },

  _onAlphaButtonTap() {
    this.refs.leftNav.toggle();
  }
});
