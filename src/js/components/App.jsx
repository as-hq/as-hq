import {logDebug} from '../AS/Logger';

import React, {PropTypes} from 'react';
import ASTreeNav from './ASTreeNav.jsx';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import ASTopBar from './ASTopBar.jsx';
import ASErrorPane from './ASErrorPane.jsx';

import U from '../AS/Util';
const {
  Conversion: TC
} = U;

import {AppCanvas, LeftNav, Paper, Styles} from 'material-ui';
import API from '../actions/ASApiActionCreators';
import Constants from '../Constants';
import SheetStateStore from '../stores/ASSheetStateStore';

import ThemeManager from 'material-ui/lib/styles/theme-manager';
import DarkTheme from 'material-ui/lib/styles/raw-themes/dark-raw-theme';

export default React.createClass({

  /* When mounting, send a message to the backend to signify a connection */
  componentWillMount() {
    let sheetId, userId;

    if (Constants.isProduction || Constants.promptUser) {
      sheetId = window.prompt("Enter the name of your sheet. Your data on this sheet will persist -- you can access it again by entering the same sheet name on this prompt when you reload AlphaSheets. \n\nNOTE: Anyone can access your sheet by typing in its name.", "INIT_SHEET_ID");
      userId = window.prompt("Enter your username.","TEST_USER_ID");
    } else {
      sheetId = "INIT_SHEET_ID";
      userId = "TEST_USER_ID";
    }

    SheetStateStore.setCurrentSheetById(sheetId);
    SheetStateStore.setUserId(userId);
    API.initialize();
  },

  getInitialState() {
    return {
      currentPane: 'eval',
      /* object passed from splash pane specifying initial params: opened sheet, etc */
      initEvalInfo: {}
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
      muiTheme: ThemeManager.getMuiTheme(DarkTheme)
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
      <div style={{width:"100%",height:"100%"}} >
        <ASTopBar toggleEvalHeader={this._toggleEvalHeader} />
        <div style={{width: '100%', height: '100%'}}>
          <div style={{display: 'inline-block', width: '100%', height: '80%', verticalAlign:'top'}}>
            <ASEvaluationPane behavior="default" ref="evalPane" initInfo={this.state.initEvalInfo} height='100%'/>
          </div>
          <div style={{display: 'inline-block', width: '100%', height: '20%'}}>
            <ASErrorPane onRequestSelect={this._handleRequestSelect}/>
          </div>
        </div>
      </div>

    );
  },



/**************************************************************************************************************************/
/* Top-level ui state changes */

  _toggleEvalHeader() {
    this.refs.evalPane.toggleEvalHeader();
  },

  _handleRequestSelect(idx) {
    let rng = TC.indexToRange(idx);
    let sel = { origin: idx, range: rng };
    this.refs.evalPane.getASSpreadsheet().select(sel);
  }
});
