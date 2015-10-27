import React from 'react';
import Constants from '../Constants';
import Util from '../AS/Util';
let TextareaAutosize = require('react-autosize-textarea');


export default React.createClass({

  /**************************************************************************************************************************/
  // React  methods

  propTypes: {
    onKeyDown: React.PropTypes.func.isRequired,
    onKeyUp: React.PropTypes.func.isRequired,
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
    this.setState({expression:xpStr});
  },

  updateTextBoxInit(xpStr) {
    this.setState({expression: xpStr, isVisible: vis});
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
  // Render

  render() {
    console.log("rendering textbox");
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
                 onKeyDown={this.props.onKeyDown}
                 onKeyUp={this.props.onKeyUp}
                 value={this.state.expression} /> : null ;

    return (
        <div style={baseStyle} >
          {overlay}
        </div>
    );
  }

});
