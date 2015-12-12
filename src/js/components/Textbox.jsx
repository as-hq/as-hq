import {logDebug} from '../AS/Logger';

import React from 'react';
import Constants from '../Constants';

import Util from '../AS/Util';
import KeyUtils from '../AS/KeyUtils';
import ShortcutUtils from '../AS/ShortcutUtils';
import ParseUtils from '../AS/ParsingUtils';

import CellStore from '../stores/ASCellStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import SelectionStore from '../stores/ASSelectionStore';
import ExpStore from '../stores/ASExpStore';
import ExpActionCreator from '../actions/ASExpActionCreators.js';

let TextareaAutosize = require('react-autosize-textarea');

var ace = require('brace');

export default React.createClass({

  /**************************************************************************************************************************/
  // React  methods

  propTypes: {
    onDeferredKey: React.PropTypes.func.isRequired,
    position: React.PropTypes.func.isRequired
  },

  getInitialState() {
    return {
      isVisible: false,
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
    // TODO: add a lang prop or something
    this.editor.getSession().setMode('ace/mode/' + this.props.mode);
    this.editor.setFontSize(12);
    this.editor.setOption('maxLines', Infinity);
    this.editor.renderer.setShowGutter(false); // no line numbers
    this.editor.getSession().setUseWrapMode(true); // no word wrap
  },

  componentWillReceiveProps(nextProps) {
    this.editor.getSession().setMode('ace/mode/' + nextProps.mode);
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
    this.editor.setValue(xpStr);
    if (cursorPos != null) {
      this.editor.moveCursorTo(0, cursorPos);
    }
    this.editor.clearSelection(); // otherwise ace highlights whole xp
  },

  hideTextBox() {
    this.setState({isVisible:false});
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
    logDebug("Inserting ref in textbox " + newRef);
    logDebug("Expression before insertion: " + this.editor.getValue());
    ExpStore.setDoTextBoxCallback(false);
    if (lastRef !== null) {
      ParseUtils.deleteLastRef(this.editor,lastRef);
    }
    this.editor.getSession().insert(this.editor.getCursorPosition(),newRef);
    logDebug("New textbox xp: " + this.editor.getValue());
  },

  /**************************************************************************************************************************/
  // Respond to events from ace

  _onKeyDown(e) {
    logDebug("\n\nTEXTBOX KEYDOWN");
    if (ShortcutUtils.textboxShouldDeferKey(e)) {
      // console.log("TEXTBOX DEFERRING KEY");
      KeyUtils.killEvent(e);
      this.props.onDeferredKey(e);
    } else {
        // onChange will call an action creator
        // you want an onchange to fire here
      ExpStore.setDoTextBoxCallback(true);
      logDebug("textbox will fire action creator", e);
    }
  },

  // Appends a % to the end of the editor string, and sets the cursor to
  // be one before the %.
  _onChange(e) {
    if (ExpStore.getDoTextBoxCallback()) {
      let xpStr = this.editor.getValue();
      logDebug("Textbox change new string: " + xpStr);
      ExpActionCreator.handleTextBoxChange(xpStr);
    }
    this.setState({renderTrigger: !this.state.renderTrigger});
  },

  _onFocus(e) {
    this.props.hideToast();
    logDebug("FOCUS ON TEXTBOX");
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
      <div id={'textbox'}
           style={baseStyle} />
    );
  }

});
