import React from 'react';
import Constants from '../Constants';

import Util from '../AS/Util';
import KeyUtils from '../AS/KeyUtils';
import ShortcutUtils from '../AS/ShortcutUtils';

import ExpStore from '../stores/ASExpStore';
import ExpActionCreator from '../actions/ASExpActionCreators.js';

let TextareaAutosize = require('react-autosize-textarea');

export default React.createClass({

  /**************************************************************************************************************************/
  // React  methods

  propTypes: {
    onDeferredKey: React.PropTypes.func.isRequired,
    position: React.PropTypes.object.isRequired
  },

  getInitialState(){
    return {
      expression: "",
      isVisible: false
    };
  },

  /**************************************************************************************************************************/
  // Text box focus and update methods

  /* Hack to get the cursor at the end upon focus */
  onFocus(){
    let curXp = this.state.expression,
        newXp = '';
    this.setState({expression:newXp},function(){
      this.setState({expression:curXp});
    })
  },

  updateTextBox(xpStr){
    console.log("Updating textbox: " + xpStr);
    this.setState({expression:xpStr, isVisible: true});
  },

  hideTextBox(){
    this.setState({isVisible:false});
  },

  getWidth(xp){
    let rows = xp.split("\n"),
        longestStr = rows.reduce(function (a, b) { return a.length > b.length ? a : b; }),
        extentX = this.props.position ? this.props.position.extent.x : 0;
    return Math.max(extentX, (longestStr.length)*12);
  },

  /**************************************************************************************************************************/
  // Respond to key events

  _onKeyDown(e){
    console.log("\n\nTEXTBOX KEYDOWN");
    e.persist(); // prevent react gc
    if (ShortcutUtils.gridShouldDeferKey(e)){ // if anything but nav keys, bubble event to parent
      if (KeyUtils.producesVisibleChar(e) && e.which !== 13) {
        console.log("Textbox keydown needs action creator");
      } 
      else { // try shortcut
        console.log("TEXTBOX DEFERRED KEY");
        this.props.onDeferredKey(e);
      }
    }
  },

  /* This will not be called upon a setState in updateTextBox because (?)
  React only fires it internally upon a user-interaction
  As a result, we don't need an extra bool in the ExpStore as with the Ace Editor
  */
  _onChange(e){
    let xpStr = e.target.value;
    console.log("TEXTBOX CHANGE: " + xpStr);
    ExpActionCreator.handleTextBoxChange(xpStr);
    this.setState({expression:xpStr});
  },

  /**************************************************************************************************************************/
  // Render

  render() {
    console.log("Textbox rendering");
    let baseStyle = {display:'block',
                     position:'absolute',
                     width:"100%",
                     top: this.props.position ? this.props.position.origin.y : null,
                     left: this.props.position ? this.props.position.origin.x : null,
                     zIndex:5
                     };
    let textStyle = {
      width: this.getWidth(this.state.expression),
      border: "solid 2px black"
    };
    let overlay = this.state.isVisible ?
          <TextareaAutosize ref="box" type="text" autoFocus onFocus={this.onFocus}
                 style={textStyle}
                 onKeyDown={this._onKeyDown}
                 onChange={this._onChange}
                 value={this.state.expression} /> : null ;
    return (
        <div style={baseStyle} >
          {overlay}
        </div>
    );
  }

});
