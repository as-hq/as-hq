import {logDebug} from '../AS/Logger';

import React, {PropTypes} from 'react';
import ASTreeNav from './ASTreeNav.jsx';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import ASTopBar from './ASTopBar.jsx';
import ASBottomBar from './ASBottomBar.jsx';
import ResizablePanel from './ResizablePanel.jsx'
import Toolbar from './toolbar/Toolbar.jsx';

import ASErrorPane from './bottom-panes/ASErrorPane.jsx';
import ASOutputPane from './bottom-panes/ASOutputPane.jsx';

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
      // object passed from splash pane specifying initial params: opened sheet, etc
      initEvalInfo: {},
      errorPaneOpen: true,
      outputPaneOpen: false
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
    let {errorPaneOpen, outputPaneOpen} = this.state;

    // 100% doesn't account for the heights of topbar or toolbar??
    let evalPaneOverflowCorrection = 85;
    let paneHeight = 180;
    let bottomBarHeight = 24;
    let evalPaneOffset = evalPaneOverflowCorrection
                        + ((errorPaneOpen || outputPaneOpen) ? paneHeight : 0)
                        + bottomBarHeight;

    let paneStyle = {width: '100%',
                      height: `${paneHeight}px`};

    return (
      <div style={{width:"100%",height:"100%"}} >
        <ASTopBar toggleEvalHeader={this._toggleEvalHeader.bind(this)} />

        <Toolbar />

        <div style={{width: '100%', height: `calc(100% - ${evalPaneOffset}px)`}}>
          <ASEvaluationPane behavior="default" ref="evalPane" initInfo={this.state.initEvalInfo} />;
        </div>

        <div style={{
          ...paneStyle,
          ...(errorPaneOpen ? { } : {'display': 'none'})}}>
          <ASErrorPane open={errorPaneOpen} onRequestSelect={this._handleRequestSelect.bind(this)}/>;
        </div>

        <div style={{
          ...paneStyle,
          ...(outputPaneOpen ? { } : {'display': 'none'})}}>
          <ASOutputPane open={outputPaneOpen} />;
        </div>

        <div style={{height: `${bottomBarHeight}px`, width: '100%'}}>
          <ASBottomBar
            toggleErrorPane={this._toggleErrorPane.bind(this)}
            toggleOutputPane={this._toggleOutputPane.bind(this)} />
        </div>
      </div>
    );
    // This ResizablePanel isn't currently working
    // <div style={{width: '100%', height: 'calc(100% - 110px)'}}> {/* supposed to be calc(100%-72px)*/}
    //   <ResizablePanel content={evalPane} sidebar={errorPane} sidebarVisible={errorPaneOpen} side="bottom" />
    // </div>
  },



/**************************************************************************************************************************/
/* Top-level ui state changes */

  _toggleEvalHeader() {
    this.refs.evalPane.toggleEvalHeader();
  },

  _toggleErrorPane() {
    this.setState({
      errorPaneOpen: ! this.state.errorPaneOpen,
      outputPaneOpen: false
    });
  },

  _toggleOutputPane() {
    this.setState({
      outputPaneOpen: ! this.state.outputPaneOpen,
      errorPaneOpen: false
    });
  },

  _handleRequestSelect(idx) {
    let rng = TC.indexToRange(idx);
    let sel = { origin: idx, range: rng };
    this.refs.evalPane.getASSpreadsheet().select(sel);
  }
});
