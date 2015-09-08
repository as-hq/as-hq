import React, {PropTypes} from 'react';
import ASTreeNav from './ASTreeNav.jsx';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import ASSplashPane from './ASSplashPane.jsx';
import {AppCanvas, LeftNav, Paper, Styles} from 'material-ui';
import ASNavBar from './ASNavBar.jsx';
import ASRibbon from './ASRibbon.jsx';
import API from '../actions/ASApiActionCreators';
import Constants from '../Constants';

const ThemeManager = new Styles.ThemeManager();

export default React.createClass({

  /* When mounting, send a message to the backend to signify a connection */
  componentWillMount() {
    ThemeManager.setTheme(ThemeManager.types.DARK);
    API.sendInitialMessage();
  },
  componentDidMount() {

  },
  getInitialState() {
    return {
      activeDocumentTab: 'test',
      activeRibbonTab: 'Home',
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
      muiTheme: ThemeManager.getCurrentTheme()
    };
  },

  /**************************************************************************************************************************/
  /* Core render method for the whole app */

  getPaneHeight() {
    console.log("WINDOW HEIGHT: " + window.innerHeight);
    return window.innerHeight;
  },

  render() {
    let leftNavMenuItems = [
      { route: 'all-files', text: 'All files' },
      { route: 'logout', text: 'Log out' }
    ];

    console.log("PANE HEIGHT: " + this.getPaneHeight());

    let panes = {
      eval: <ASEvaluationPane behavior="default" ref="evalPane" initInfo={this.state.initEvalInfo} height={this.getPaneHeight()}/>,
      splash: <ASSplashPane onProceed={this._onPaneChange} height={this.getPaneHeight()}/>
    };

    return (
       <div style={{width:"100%",height:"100%"}} >
        <LeftNav
          ref="leftNav"
          menuItems={leftNavMenuItems}
          docked={false}/>
        <ASNavBar
          onDocumentTabChange={this._onDocumentTabChange}
          onRibbonTabChange={this._onRibbonTabChange}
          onAlphaButtonTap={this._onAlphaButtonTap}/>
        <ASRibbon activeTab={this.state.activeRibbonTab} />
        <div style={{width: '100%', height: this.getPaneHeight()}}>
          <div style={{display: 'inline-block', width: '10%', height: '100%', verticalAlign: 'top'}}>
            <ASTreeNav
              onDocumentOpen={this._onDocumentOpen}
              onSheetCreate={this._onSheetCreate}
              onWorkbookCreate={this._onWorkbookCreate}/>
          </div>
          <div style={{display: 'inline-block', width:'90%', height:'100%',verticalAlign:'top'}}>
            {panes[this.state.currentPane]}
          </div>
        </div>
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
  },

  _onPaneChange(pane, initInfo) {
    switch(pane) {
      case 'eval':
        this.setState({ currentPane: pane, initEvalInfo: initInfo });
        break;
    }
  },

  _onDocumentOpen(documentId) {
    //TODO
    console.log('App on document open', documentId);
  },

  _onSheetCreate() {
    //TODO
    console.log('Created sheet');
  },

  _onWorkbookCreate() {
    //TODO
    console.log('Created workbook');
  }
});
