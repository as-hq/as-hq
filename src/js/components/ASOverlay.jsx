import React from 'react';
import Constants from '../Constants';

export default React.createClass({
  getInitialState(){
    return {
      textVal:this.props.overlay.xp
    };
  },
  componentWillMount() {
  },
  
  getX() {
    return (this.props.overlay.col-1) * Constants.cellWidthPx- this.props.scroll.x  + Constants.gridXOffset;
  },

  getY() {
    return (this.props.overlay.row-1) * Constants.cellHeightPx  - this.props.scroll.y + Constants.gridYOffset;
  },

  isVisible() {
    return this.props.isVisible(this.props.overlay.col, this.props.overlay.row);
  },

  textBoxHandleChange(e){
    this.setState({textVal:e.target.value,function(e){
      this.props.textBoxChange(e.target.value);
    }});
  },
  updateVal(e){
    this.setState({textVal:e.target.value});
  },
  getWidth(len){
    return Math.max(50,30+len*12);
  },

  render() {
    console.log("Text box: " + this.props.overlay.xp);
    let baseStyle = {display:'block',
                     position:'absolute',
                     width: this.props.overlay.width,
                     height: this.props.overlay.height,
                     top: this.getY(),
                     left: this.getX(),
                     visibility: this.isVisible() ? 'visible' : 'hidden',
                     zIndex: 5};

    let overlay;
    let textStyle = { WebkitUserSelect:"auto",
                     width:this.getWidth(this.props.overlay.xp.length),
                     outlineStyle:"solid",
                     outlineWidth:"2px"
                     };
    switch(this.props.overlay.tag) {
      case "ValueImage":
        overlay = (<image src={this.props.overlay.src} width="100%" height="100%" alt="Error rendering image." />);
        break;
      case "TextBox":
        overlay = (<input ref="textbox" type="text" style={textStyle} onKeyDown={this.textBoxHandleChange} value={this.state.textVal} onChange={this.updateVal}/>);
        break;
    }
    return (
        <div style={baseStyle} >
          {overlay}
        </div>
    );
  }

});
