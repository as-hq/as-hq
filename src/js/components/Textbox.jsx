import React from 'react';
let TextareaAutosize = require('react-autosize-textarea');
let ace = require('brace');

import Constants from '../Constants';
import U from '../AS/Util';
import {logDebug} from '../AS/Logger';

import CellStore from '../stores/ASCellStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import SelectionStore from '../stores/ASSelectionStore';
import ExpStore from '../stores/ASExpStore';
import ExpActionCreator from '../actions/ASExpActionCreators.js';

export default React.createClass({

  /**************************************************************************************************************************/
  // React methods

  propTypes: {
    onDeferredKey: React.PropTypes.func.isRequired,
    position: React.PropTypes.func.isRequired
  },

  getInitialState() {
    return {
      isVisible: false,
      language: Constants.Languages.Excel,
      renderTrigger: false // WTF we have to do this I feel so dirty
    };
  },

  componentDidMount() {
    this.editor = ace.edit('textbox');
    this.editor.$blockScrolling = Infinity;
    this.editor.on('focus', this._onFocus);
    this.editor.getSession().on('change', this._onChange);
    this.editor.container.addEventListener('keydown',this._onKeyDown,true);
    this.showCursor();
    this.editor.setFontSize(12);
    this._updateMode(this.state.language);
    this.editor.setOption('maxLines', Infinity);
    this.editor.renderer.setShowGutter(false); // no line numbers
    this.editor.getSession().setUseWrapMode(true); // no word wrap
  },

  componentWillReceiveProps(nextProps) {

  },

  // When the component is about to update (after the initial render), update the mode of the editor as well
  // This has the effect of enabling autocomplete/syntax highlighting for that language, among other things
  componentWillUpdate(nextProps, nextState) {
    if (nextState.language !== this.state.language) {
      this._updateMode(nextState.language);
    }
  },

  _updateMode(lang) {
    if (this.editor) {
      this.editor.getSession().setMode('ace/mode/' + Constants.AceMode[lang]);
    }
  },

  /**************************************************************************************************************************/
  // Text box focus and update methods

  // null/undefined cursorPos means selection goes to the end
  updateTextBox(xpStr, cursorPos) {
    logDebug("Updating textbox: " + xpStr);
    ExpStore.setDoTextBoxCallback(false);
    if (!this.state.isVisible) { //will be visible after update, put cursor in textbox
      this.showCursor();
    }
    this.setState({isVisible: true});
    this.updateLanguage();
    this.editor.setValue(xpStr);
    if (cursorPos != null) {
      this.editor.moveCursorTo(0, cursorPos);
    }
    this.editor.clearSelection(); // otherwise ace highlights whole xp
  },

  hideTextBox() {
    this.setState({isVisible: false});
  },

  // Make sure that the language and consequently the mode of the textbox is in sync with the ExpStore
  updateLanguage() {
    let lang = ExpStore.getLanguage();
    if (this.state.language !== lang) {
      this.setState({language: lang});
    }
  },

  showCursor() {
    this.editor.renderer.$cursorLayer.showCursor(); // blinking cursor on textbox
  },

  getWidth() {
    if (SelectionStore.getActiveSelection()) {
      let xp = this.editor.getValue(),
          rows = xp.split("\n"),
          longestStr = rows.reduce(function (a, b) { return a.length > b.length ? a : b; }),
          extentX = this.props.position() ? this.props.position().extent.x : 0;
      return Math.max(extentX, (longestStr.length)*7);
    } else {
      return 0;
    }
  },

  getRawEditor() {
    return this.editor;
  },

  isVisible() {
    return this.state.isVisible;
  },

  /**************************************************************************************************************************/
  // Helpers

  insertRef(newRef) {
    let lastRef = ExpStore.getLastRef();
    ExpStore.setDoTextBoxCallback(false);
    if (lastRef !== null) {
      U.Parsing.deleteLastRef(this.editor,lastRef);
    }
    this.editor.getSession().insert(this.editor.getCursorPosition(),newRef);
  },

  /**************************************************************************************************************************/
  // Respond to events from ace

  _onKeyDown(e) {
    if (U.Shortcut.textboxShouldDeferKey(e)) {
      // console.log("TEXTBOX DEFERRING KEY");
      U.Key.killEvent(e);
      this.props.onDeferredKey(e);
    } else {
        // onChange will call an action creator
        // you want an onchange to fire here
      ExpStore.setDoTextBoxCallback(true);
    }
  },

  // Appends a % to the end of the editor string, and sets the cursor to
  // be one before the %.
  _onChange(e) {
    if (ExpStore.getDoTextBoxCallback()) {
      let xpStr = this.editor.getValue();
      ExpActionCreator.handleTextBoxChange(xpStr);
    }
    this.setState({renderTrigger: !this.state.renderTrigger});
  },

  _onFocus(e) {
    this.props.hideToast();
    this.props.setFocus('textbox');
    ExpStore.setLastCursorPosition(Constants.CursorPosition.TEXTBOX);
    ExpStore.setLastRef(null);
    this.showCursor();
    this.editor.resize();
  },

  /**************************************************************************************************************************/
  // Render

  render() {
    let baseStyle = {
      display:'block',
      position:'absolute',
      // height of each line in the editor, add 5 to give some leeway at top and bottom
      lineHeight: this.props.position() ? this.props.position().extent.y + 5 + "px" : "20px",
      top: this.props.position() ? this.props.position().origin.y : null,
      left: this.props.position() ? this.props.position().origin.x : null,
      zIndex: 5,
      width: this.getWidth(),
      // height is a function of lineHeight and maxLines, see ace virtual renderer
      border: "solid 2px black",
      visibility: this.state.isVisible ? 'visible' : 'hidden'
    };
    return (
      <div id={'textbox'} style={baseStyle} />
    );
  }

});
