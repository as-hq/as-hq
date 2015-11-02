import React from 'react';
import Constants from '../Constants';

import Util from '../AS/Util';
import KeyUtils from '../AS/KeyUtils';
import ShortcutUtils from '../AS/ShortcutUtils';
import ParseUtils from '../AS/ParsingUtils';

import Store from '../stores/ASEvaluationStore';
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

  getInitialState(){
    return {
      isVisible: false,
      renderTrigger: false // WTF we have to do this I feel so dirty
    };
  },

  componentDidMount(){
    this.editor = ace.edit('textbox');
    this.editor.$blockScrolling = Infinity;
    this.editor.on('focus', this._onFocus);
    this.editor.getSession().on('change', this._onChange);
    this.editor.container.addEventListener('keydown',this._onKeyDown,true);
    this.showCursor();
    // TODO: add a lang prop or something
    this.editor.getSession().setMode('ace/mode/python');
    this.editor.setFontSize(12);
    this.editor.setOption('maxLines', Infinity);
    this.editor.renderer.setShowGutter(false); // no line numbers
    this.editor.getSession().setUseWrapMode(true); // no word wrap
  },

  /**************************************************************************************************************************/
  // Text box focus and update methods

  updateTextBox(xpStr){
    console.log("Updating textbox: " + xpStr);
    ExpStore.setDoTextBoxCallback(false);
    if (!this.state.isVisible){ //will be visible after update, put cursor in textbox
      this.showCursor();
    }
    
    this.setState({isVisible: true});
    this.editor.setValue(xpStr);
    this.editor.clearSelection(); // otherwise ace highlights whole xp
    this.props.hideToast();
  },

  hideTextBox(){
    this.setState({isVisible:false});
  },

  showCursor(){
    this.editor.renderer.$cursorLayer.showCursor(); // blinking cursor on textbox
  },

  getWidth(){
    if (Store.getActiveSelection()){
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

  insertRef(newRef){
    let lastRef = ExpStore.getLastRef();
    console.log("Inserting ref in textbox " + newRef);
    console.log("Expression before insertion: " + this.editor.getValue());
    ExpStore.setDoTextBoxCallback(false);
    if (lastRef !== null){
      ParseUtils.deleteLastRef(this.editor,lastRef);
    }
    this.editor.getSession().insert(this.editor.getCursorPosition(),newRef);
    console.log("New textbox xp: " + this.editor.getValue());
  },

  /**************************************************************************************************************************/
  // Respond to events from ace

  _onKeyDown(e){
    console.log("\n\nTEXTBOX KEYDOWN");
    if (ShortcutUtils.textboxShouldDeferKey(e)) {
      KeyUtils.killEvent(e);
      this.props.onDeferredKey(e);
    } else {
        // onChange will call an action creator
        // you want an onchange to fire here
      ExpStore.setDoTextBoxCallback(true);
      console.log("textbox will fire action creator", e);
    }
  },

  _onChange(e){
    let xpStr = this.editor.getValue();
    if (ExpStore.getDoTextBoxCallback()){
      console.log("Textbox change new string: " + xpStr);
      ExpActionCreator.handleTextBoxChange(xpStr);
    }
    this.setState({renderTrigger: !this.state.renderTrigger});
  },

  _onFocus(e) {
    console.log("FOCUS ON TEXTBOX");
    Store.setFocus('textbox');
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
