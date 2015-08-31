import React from 'react';

export default React.createClass({
  getDefaultProps(){
    return {
      height:'110px'
    }
  },
  render: function() {
    let defaultPad = 2;
    let children = [];
    if (this.props.children) { // deal with only one child
      if (!Array.isArray(this.props.children)){
        this.props.children = [this.props.children];
      }
      let sumHeights = 0;
      let numChildren = 0;
      for (var key in this.props.children){
        if (this.props.children[key].props.height){
          sumHeights += parseFloat(this.props.children[key].props.height);
          numChildren += 1;
        }
      }
      for (var key in this.props.children) {
        /* All child elements of a col inherit its width for simplicity */
        let child = React.cloneElement(this.props.children[key],{width:this.props.width});

        if (key == 0){
          let firstStyle = {};
          let pt = (parseFloat(this.props.height)-sumHeights-(numChildren-1)*defaultPad)/2 + 'px';
          if (this.props.children.length>1){
            firstStyle = {
              paddingTop: pt
            };
          }
          else {
            firstStyle = {
              paddingTop: pt,
              paddingBottom: pt
            };
          }
          children.push(<div style={firstStyle}>{child}</div>);
          console.log("COL " + sumHeights + " " + JSON.stringify(firstStyle));
        }
        else if (key == this.props.children.length-1){
          let pb = (parseFloat(this.props.height)-sumHeights-(numChildren-1)*defaultPad)/2 + 'px';
          let firstStyle = {
            paddingTop: defaultPad + 'px',
            paddingBottom: pb
          };
          children.push(<div style={firstStyle}>{child}</div>);
          console.log("COL " + sumHeights + " " + JSON.stringify(firstStyle));
        }
        else {
          let divStyle = {
            paddingTop: defaultPad + 'px'
          };
          children.push(<div style={divStyle}>{child}</div>);
          console.log("COL " + sumHeights + " " + JSON.stringify(divStyle));
        }
      }
    }
    let colDivStyle={ // no padding left needed; section will take care of it
      position:'relative',
      textAlign:'center',
      verticalAlign:'middle',
      display:'inline-block',
      margin: '0px 0px 0px 10px',
      height: this.props.height,
      width: this.props.width
    }
    return (
      <div style={colDivStyle}>{children}</div>
    );
  }

});
