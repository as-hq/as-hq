import React from 'react';
import Constants from '../Constants';

export default React.createClass({
  componentWillMount() {
  },

  getX() {
    return (this.props.overlay.col - this.props.scroll.x) * Constants.cellWidthPx + Constants.gridXOffset;
  },

  getY() {
    return (this.props.overlay.row - this.props.scroll.y) * Constants.cellHeightPx + Constants.gridYOffset;
  },

  isVisible() {
    return this.props.isVisible(this.props.overlay.col, this.props.overlay.row);
  },

  render() {
    let self = this;
    let baseStyle = {display:'block',
                     position:'absolute',
                     width: this.props.overlay.width,
                     height: this.props.overlay.height,
                     top: self.getY(),
                     left: self.getX(),
                     visibility: self.isVisible() ? 'visible' : 'hidden',
                     zIndex: 5};
    let overlay;
    switch(this.props.overlay.tag) {
      case "ValueImage":
        overlay = (<image src={this.props.overlay.src} width="100%" height="100%" alt="Error rendering image." />);
        break;
    }
    return (
        <div style={baseStyle} >
          {overlay}
        </div>
    );
  }

});
