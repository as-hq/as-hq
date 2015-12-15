import React from 'react';
import Constants from '../Constants';
let Draggable = require('react-draggable');
let ReactDOM = require('react-dom');
import API from '../actions/ASApiActionCreators';
import CellStore from '../stores/ASCellStore';

export default React.createClass({

  updateImageProps(prop) {
    let rng = {tl:this.props.overlay.loc.index, br:this.props.overlay.loc.index};
    API.setProp(prop, rng);
  },

  _onStop(e, detail) {
    // ImageData shouldn't leak into this part of the code; should probably
    // create an ImageData type with imageOffsetX, etc., and pass that around,
    // and only insert tag: "ImageData" in the API. (Alex 12/15)
    let prop = {
      tag: "ImageData",
      imageOffsetX: parseFloat(detail.position.left) + this.props.overlay.offsetX,
      imageOffsetY: parseFloat(detail.position.top) + this.props.overlay.offsetY,
      imageWidth: this.props.overlay.width,
      imageHeight: this.props.overlay.height
    };

    if (detail.position.left !== 0 || detail.position.top !== 0) {
      this.updateImageProps(prop);
    }
  },

  _onMouseUp(e) {
    let node = ReactDOM.findDOMNode(this.refs.image);
    if (node !== null) {
      let {width,height} = node.style,
          tagValue = {
            tag: "ImageData",
            imageOffsetX: this.props.overlay.offsetX,
            imageOffsetY: this.props.overlay.offsetY,
            imageWidth: parseFloat(width),
            imageHeight: parseFloat(height)
          };
      if (tagValue.imageWidth !== this.props.overlay.width || tagValue.imageHeight !== this.props.overlay.height) {
        this.updateImageProps(tagValue);
      }
    }
  },

  render() {
    let baseStyle = {
      position:'absolute',
      width: this.props.overlay.width,
      height: this.props.overlay.height,
      top: this.props.overlay.top + this.props.overlay.offsetY,
      left: this.props.overlay.left + this.props.overlay.offsetX,
      zIndex:5,
      resize:'auto',
      overflow:'hidden',
      border: "solid 2px black",
    };
    let overlay = <image src={this.props.overlay.src} draggable="false" width="100%" height="100%" alt="Error rendering image." />
    // onMouseUp doesn't work inside draggable
    return (
      <div onMouseUp={this._onMouseUp}>
        <Draggable
          moveOnStartChange={false}
          start={{x: 0, y: 0}}
          zIndex={100}
          onStop={this._onStop}>
          <div style={baseStyle} ref="image" >
            {overlay}
          </div>
        </Draggable>
      </div>
    );
  }

});
