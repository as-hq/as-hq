import React from 'react';
import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';


export default React.createClass({

  getInitialState(){
    return {
      textBox:null
    };
  },

  updateTextBox(xp,isTyping){
    console.log("Updating text box in textbox component.");
    let col = Store.getActiveSelection().range.col,
        row = Store.getActiveSelection().range.row,
        textBox = {col: col, row: row, xp:xp};
    if (isTyping)
      this.setState({textBox:textBox});
    else
      this.setState({textBox:null});
  },

  textBoxChange(e){
    this.props.textBoxChange(e.target.value);
  },

  getWidth(xp){
    let rows = xp.split("\n"),
        longestStr = rows.reduce(function (a, b) { return a.length > b.length ? a : b; });
    return Math.max(50,30+(longestStr.length)*12);
  },

  getHeight(xp){
    let numCols = xp.split("\n").length;
    return 1.5*numCols*Constants.cellHeightPx;
  },

  render() {
    if (this.state.textBox)
      console.log("RENDERING TEXTBOX " + this.state.textBox.xp + " " + 
        this.state.textBox.col + " "+ this.state.textBox.row);
    let baseStyle = {display:'block',
                     position:'absolute',
                     top: this.state.textBox ? Util.getY(this.state.textBox.row,this.props.scroll.y) : 0,
                     left: this.state.textBox ? Util.getX(this.state.textBox.col,this.props.scroll.x) : 0,
                     zIndex:5
                     };
    let textStyle = {
      width: this.state.textBox ? this.getWidth(this.state.textBox.xp) : 0,
      height: this.state.textBox ? this.getHeight(this.state.textBox.xp) : 0,
      border: "solid 3px black"
    };

    let  overlay = this.state.textBox ? 
          <input type="text" 
                 style={textStyle}
                 onKeyDown={this.props.onKeyDown}
                 value={this.state.textBox.xp} 
                 onChange={this.textBoxChange}/> : null ;
    
    return (
        <div style={baseStyle} >
          {overlay}
        </div>
    );
  }

});
