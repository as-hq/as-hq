import React from 'react';

export default React.createClass({
  getDefaultProps(){
    return {
      height:'25px'
    }
  },
  render: function() {
    let defaultPad = 2;
    let children = [];
    if (this.props.children) { // deal with only one child
      if (!Array.isArray(this.props.children)){
        this.props.children = [this.props.children];
      }
      for (var key in this.props.children) {
        let child = this.props.children[key];
        let divStyle = {
          display: 'inline-block',
          paddingLeft: defaultPad + 'px'
        };
        children.push(<div style={divStyle}>{child}</div>);
      }
    }
    let rowDivStyle={
      textAlign:'center',
      height: this.props.height,
      width: this.props.width
    };
    return (
      this.props.children ? <div style={rowDivStyle}>{children}</div> : <div style={rowDivStyle} />
    );
  }
  
});
