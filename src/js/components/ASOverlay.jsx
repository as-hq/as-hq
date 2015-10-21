import React from 'react';
import Constants from '../Constants';
import Util from '../AS/Util';


export default React.createClass({

  isVisible() {
    return this.props.isVisible(this.props.overlay.col, this.props.overlay.row);
  },


  render() {
    let baseStyle = {display:'block',
                     position:'absolute',
                     width: this.props.overlay.width,
                     height: this.props.overlay.height,
                     top: Util.getY(this.props.overlay.row,this.props.scroll.y),
                     left: Util.getX(this.props.overlay.col,this.props.scroll.x),
                     visibility: this.isVisible() ? 'visible' : 'hidden',
                     zIndex:5
                     };

    let overlay;
    switch(this.props.overlay.tag) {
      case "ValueImage":
        overlay = (<image src={this.props.overlay.src} width="100%" height="100%" alt="Error rendering image." />);
        // console.log("\n\n\n IMAGE " + baseStyle.top + " " + baseStyle.left);
        break;
    }
    return (
        <div style={baseStyle} >
          {overlay}
        </div>
    );
  }

});
