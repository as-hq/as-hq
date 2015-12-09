import React from 'react';
import {AppBar, FlatButton, Styles} from 'material-ui';

import API from '../actions/ASApiActionCreators';
import Constants from '../Constants';
import CellStore from '../stores/ASCellStore';

import ASButton from './basic-controls/ASButton.jsx';
import ASCondFormattingDialog from './dialogs/ASCondFormattingDialog.jsx';
import ASFileImportButton from './ASFileImportButton.jsx';

let {Colors} = Styles;

export default React.createClass({
  getInitialState() {
    return ({
      condFormattingOpen: false
    });
  },

  render() {
    return (
      <AppBar
        style={{
          backgroundColor: Colors.grey900
        }}
        onLeftIconButtonTouchTap={this._onAlphaButtonTap}>
        <ASCondFormattingDialog
          open={this.state.condFormattingOpen}
          onRequestClose={this._onCondFormatClose}/>
        <div style={{width: '250px', maxWidth: '250px', display: 'inline-block'}} />
        <FlatButton
          label="HEADER"
          onClick={this._toggleEvalHeader} />
        <FlatButton
          label="COND. FORMATTING"
          onClick={this._onCondFormatOpen} />
        <FlatButton
          label="SUBMIT BUG REPORT"
          onClick={this._submitBugReport} />
        <FlatButton
          label="SAVE SHEET"
          onClick={this._onSaveFile} />
        <FlatButton
          label="OPEN SHEET"
          onClick={this._onOpenFile} />
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

  _onCondFormatOpen() {
    this.setState({ condFormattingOpen: true });
  },

  _onCondFormatClose() {
    this.setState({ condFormattingOpen: false });
  },

  _onSaveFile() {
    API.export(SheetStateStore.getCurrentSheet());
  },

  _onOpenFile() {
    alert("To open a saved AlphaSheets sheet, drag it onto the spreadsheet on this page. (Cut us some slack, this is an MVP.)");
  },

  _onAlphaButtonTap() {

  }
});
