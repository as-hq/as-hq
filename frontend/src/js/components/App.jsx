/* @flow */

import type {
  Callback
} from '../types/Base';

import type {
  StoreLink
} from '../types/React';

import {logDebug} from '../AS/Logger';

import React, {PropTypes} from 'react';
import ASTreeNav from './ASTreeNav.jsx';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import ASTopBar from './ASTopBar.jsx';
import ASConnectionBar from './ASConnectionBar.jsx';
import ASBottomBar from './ASBottomBar.jsx';

import ASCondFormattingDialog from './cond-formatting/ASCondFormattingDialog.jsx';
import ASChartDialog from './chart/ASChartDialog.jsx';

import ResizablePanel from './ResizablePanel.jsx';
import Toolbar from './toolbar/Toolbar.jsx';

import ASErrorPane from './bottom-panes/ASErrorPane.jsx';
import ASCellPane from './bottom-panes/ASCellPaneController.jsx';
import ASHeaderPane from './bottom-panes/ASHeaderPaneController.jsx';

import U from '../AS/Util';
const {
  Conversion: TC
} = U;

import ASIndex from '../classes/ASIndex';
import * as flex from '../styles/flex';

// $FlowFixMe: missing annotations
import {AppCanvas, LeftNav, Paper} from 'material-ui';

import API from '../actions/ASApiActionCreators';
import DialogActions from '../actions/DialogActionCreators';
import OverlayActions from '../actions/ASOverlayActionCreators';

import Constants from '../Constants';
import SheetStateStore from '../stores/ASSheetStateStore';
import ConnectionStore from '../stores/ASConnectionStore';
import ModalStore from '../stores/ASModalStore';

// $FlowFixMe: missing annotations
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe: missing annotations
import DarkTheme from 'material-ui/lib/styles/raw-themes/dark-raw-theme';

type BottomPane = 'header' | 'error' | 'cell';

type AppState = {
  currentPane: string; // TODO this really really should be refactored out
  currentBottomPane: ?BottomPane;
  // object passed from splash pane specifying initial params: opened sheet, etc
  initEvalInfo: any;
  isConnected: boolean;
};

export default React.createClass({
  $storeLinks: ([]: Array<StoreLink>),

  /* When mounting, send a message to the backend to signify a connection */
  componentWillMount() {
    let sheetId, userId;
    const promptUser = !(Constants.sheetId || Constants.userId);

    if (Constants.isProduction || promptUser) {
      sheetId = window.prompt("Enter the name of your sheet. Your data on this sheet will persist -- you can access it again by entering the same sheet name on this prompt when you reload AlphaSheets. \n\nNOTE: Anyone can access your sheet by typing in its name.", "INIT_SHEET_ID");
      userId = window.prompt("Enter your username.","TEST_USER_ID");
    } else {
      sheetId = Constants.sheetId || "INIT_SHEET_ID";
      userId = Constants.userId || "TEST_USER_ID";
    }

    SheetStateStore.setCurrentSheetById(sheetId);
    SheetStateStore.setUserId(userId);
    ConnectionStore.addChangeListener(() => this._onConnectionStateChange());
    API.initialize();
  },

  componentDidMount() {
    U.React.addStoreLinks(this, [
      { store: ModalStore }
    ]);

    window.addEventListener('contextmenu', (evt) => {
      evt.preventDefault();
    });
  },

  componentWillUnmount() {
    U.React.removeStoreLinks(this);
  },

  getInitialState(): AppState {
    return {
      currentPane: 'eval',
      // object passed from splash pane specifying initial params: opened sheet, etc
      initEvalInfo: {},
      currentBottomPane: null,
      isConnected: true
    }
  },

  childContextTypes: {
    muiTheme: React.PropTypes.object
  },

  getChildContext(): any {
    return {
      muiTheme: ThemeManager.getMuiTheme(DarkTheme)
    };
  },

  /**************************************************************************************************************************/
  /* Core render method for the whole app */

  render(): React.Element {
    // TODO: these heights should be in a config file, not here
    const {currentBottomPane, isConnected} = this.state;
    const bottomBarHeight = 24;
    const topBarHeight = 60;
    const toolbarHeight = 50;
    const connectionBarHeight = isConnected ? 0 : 24; // #needsrefactor would be better to use flexbox than a conditional height.
    const fullStyle = {width: '100%', height: '100%'};

    const connectionBarStyle = {
      width: '100%',
      height: connectionBarHeight,
      display: isConnected ? 'none' : 'block'
    };

    // Note: it's OK to give things inside ResizablePanel height 100% because
    // ResizablePanel uses its own height as reference.
    //
    // Here, the height of the resizable panel is everything except top and
    // bottom parts, so all percents in fullStyle are relative to that
    //
    // Also, it's essential to keep the outputPane component in the layout, and
    // to make it invisible if necessary, rather than null
    let evalPane =
      <div style={styles.eval}>
        <ASEvaluationPane behavior="default" ref="evalPane" initInfo={this.state.initEvalInfo} />
      </div>;

    let bottomPane =
      <div style={fullStyle}>
        <div style={{
          ...fullStyle,
          ...(currentBottomPane === 'error' ? { } : {'display': 'none'})}}>
          <ASErrorPane
            onRequestSelect={idx => this._handleRequestSelect(idx)}
            errors={[]}
          />
        </div>

        <div style={{
          ...fullStyle,
          ...(currentBottomPane === 'cell' ? { } : {'display': 'none'})}}>
          <ASCellPane />
        </div>

        <div style={{
          ...fullStyle,
          ...(currentBottomPane === 'header' ? { } : {'display': 'none'})}}>
          <ASHeaderPane />
        </div>
      </div>;

    return (
      <div style={styles.app} >
        <div style={isConnected ? {display: 'none'} : styles.connectionBar}>
          <ASConnectionBar />
        </div>

        <ASCondFormattingDialog
          open={ModalStore.getCondFormattingOpen()}
          onRequestClose={() => DialogActions.closeCondFormattingDialog()} />
        <ASChartDialog
          open={ModalStore.getChartingOpen()}
          onRequestClose={() => DialogActions.closeChartingDialog()}
          onCreate={(chart) => OverlayActions.add(chart)} />
        <ASTopBar toggleEvalHeader={() => this._toggleEvalHeader()} />

        <Toolbar />

        <div style={{width: '100%', height: `calc(100% - ${toolbarHeight + topBarHeight + connectionBarHeight}px)`}}>
          <ResizablePanel content={evalPane}
                          sidebar={bottomPane}
                          sidebarVisible={!! currentBottomPane}
                          side="bottom" />
        </div>

        <div style={{width: '100%', height: `${bottomBarHeight}px`}} >
          <ASBottomBar toggleBottomPane={(pane: BottomPane) =>
                                      this._toggleBottomPane(pane)} />
        </div>

      </div>
    );

  },



/**************************************************************************************************************************/
/* Top-level ui state changes */

  _toggleEvalHeader() {
    this.refs.evalPane.toggleEvalHeader();
  },

  _toggleBottomPane(pane: BottomPane) {
    const {currentBottomPane} = this.state;
    if (pane !== currentBottomPane) {
      this.setState({currentBottomPane: pane});
    } else {
      this.setState({currentBottomPane: null});
    }
  },

  _onConnectionStateChange() {
    const isConnected = ConnectionStore.getIsConnected();
    this.setState({isConnected: isConnected});
  },

  _handleRequestSelect(idx: ASIndex) {
    this.refs.evalPane.getASSpreadsheet().selectIndex(idx);
  }
});


const bottomBarHeight = 24;
const topBarHeight = 60;
const toolbarHeight = 50;
const connectionBarHeight = 24;

const styles = {
  app: {
    ...flex.column,
    height: '100%',
  },
  eval: {
    height: '100%',
  },
  errorAndOutputPane: {
    height: '100%',
  },
  errorPane: {
    height: '100%',
  },
  outputPane: {
    height: '100%',
  },
  connectionBar: {
    height: connectionBarHeight,
  },
  resizable: {
    ...flex.column,
    height: '100%',
  },
  bottomBar: {
    height: bottomBarHeight,
  },
};
