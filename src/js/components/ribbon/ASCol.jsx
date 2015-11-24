import React from 'react';

export default React.createClass({
  getDefaultProps() {
    return {
      height:'110px'
    }
  },
  render() {
    let defaultPad = 2;
    let children = [];
    if (this.props.children) { // deal with only one child
      let premapChildren = this.props.children;
      if (!Array.isArray(premapChildren)) {
        premapChildren = [premapChildren];
      }
      let sumHeights = 0;
      let numChildren = 0;
      for (var key in premapChildren) {
        if (premapChildren[key].props.height) {
          sumHeights += parseFloat(premapChildren[key].props.height);
          numChildren += 1;
        }
      }
      for (var key in premapChildren) {
        /* All child elements of a col inherit its width for simplicity */
        let child = React.cloneElement(premapChildren[key],{width:this.props.width});

        if (key == 0) {
          let firstStyle = {};
          let pt = (parseFloat(this.props.height)-sumHeights-(numChildren-1)*defaultPad)/2 + 'px';
          if (premapChildren.length>1) {
            firstStyle = {
              paddingTop: pt
            };
          } else {
            firstStyle = {
              paddingTop: pt,
              paddingBottom: pt
            };
          }
          children.push(<div style={firstStyle}>{child}</div>);
          // console.log("COL " + sumHeights + " " + JSON.stringify(firstStyle));
        } else if (key == premapChildren.length-1) {
          let pb = (parseFloat(this.props.height)-sumHeights-(numChildren-1)*defaultPad)/2 + 'px';
          let firstStyle = {
            paddingTop: defaultPad + 'px',
            paddingBottom: pb
          };
          children.push(<div style={firstStyle}>{child}</div>);
          // console.log("COL " + sumHeights + " " + JSON.stringify(firstStyle));
        } else {
          let divStyle = {
            paddingTop: defaultPad + 'px'
          };
          children.push(<div style={divStyle}>{child}</div>);
          // console.log("COL " + sumHeights + " " + JSON.stringify(divStyle));
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
