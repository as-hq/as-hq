import React from 'react';
import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';
let TextareaAutosize = require('react-autosize-textarea');

/*
TODO: REFACTOR
This component uses pixels to calculate the positioning from the top left of spreadsheet. 
I don't see a way with percents right now
Can't find a good expanding component that works
I don't think these are too pressing right now
Fixed black hole doom by accounting for scrolling
--Ritesh 10/12 */


export default React.createClass({

  getInitialState(){
    return {
      textBox:null
    };
  },

  /* Update the textbox from the eval pane */
  updateTextBox(xp,isTyping){
    let col = Store.getActiveSelection().range.col,
        row = Store.getActiveSelection().range.row,
        textBox = {col: col, row: row, xp:xp};
    if (isTyping){
      this.setState({textBox:textBox});
    }
    else{
      this.setState({textBox:null});
    }
  },

  getTop(){
    if (this.state.textBox){
      return (this.state.textBox.row-1-this.props.scroll.y)* Constants.cellHeightPx + Constants.gridYOffset;
    }
    else {
      return 0; 
    }
  },

  getLeft(){
    if (this.state.textBox){
      return (this.state.textBox.col-1-this.props.scroll.x)* Constants.cellWidthPx + Constants.gridXOffset;
    }
    else {
      return 0; 
    }
  },

  getWidth(xp){
    let rows = xp.split("\n"),
        longestStr = rows.reduce(function (a, b) { return a.length > b.length ? a : b; });
    return Math.max(50,30+(longestStr.length)*12);
  },

  render() {
    console.log(JSON.stringify(this.props.scroll));
    let baseStyle = {display:'block',
                     position:'absolute',
                     width:"100%",
                     top: this.getTop(),
                     left: this.getLeft(),
                     zIndex:5
                     };
    let textStyle = {
      width: this.state.textBox ? this.getWidth(this.state.textBox.xp) : 0,
      border: "solid 2px black"
    };

    /* TODO: using autofocus has the bug that the cursor is at the beginning of the box
    at the start */
    let overlay = this.state.textBox ? 
          <TextareaAutosize ref="box" type="text" autoFocus onFocus={this.onFocus}
                 style={textStyle}
                 onKeyDown={this.props.onKeyDown}
                 value={this.state.textBox.xp} /> : null ;
    
    return (
        <div style={baseStyle} >
          {overlay}
        </div>
    );
  }

});
