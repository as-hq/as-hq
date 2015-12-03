import React from 'react';
import {AppBar, FlatButton, Styles} from 'material-ui';

import API from '../actions/ASApiActionCreators';
import Constants from '../Constants';

import ASButton from './basic-controls/ASButton.jsx';
import ASFileImportButton from './ASFileImportButton.jsx';

let {Colors} = Styles;

export default React.createClass({
  render() {
    return (
      <AppBar
        style={{
          backgroundColor: Colors.grey900
        }}
        onLeftIconButtonTouchTap={this._onAlphaButtonTap}>
        <div style={{width: '250px', maxWidth: '250px', display: 'inline-block'}} />
        <FlatButton
          label="HEADER"
          onClick={this._toggleEvalHeader} />
        <FlatButton
          label="SUBMIT BUG REPORT"
          onClick={this._submitBugReport} />
        {
          Constants.isProduction
            ? null
            : <FlatButton
                label="TEST ALPHASHEETS"
                onClick={this._testAlphasheets} />
        }
      </AppBar>
    );
  },

  _toggleEvalHeader() {
    this.props.toggleEvalHeader();
  },

  _submitBugReport() {
    let bugReport = window.prompt("Please describe the bug you encountered.","");
    API.bugReport(bugReport);
  },

  _testAlphasheets() {
    window.test();
  },

  _importFile() {

  },

  _onAlphaButtonTap() {

  }
});
