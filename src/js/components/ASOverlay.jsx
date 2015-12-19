import React from 'react';
import Constants from '../Constants';
let Draggable = require('react-draggable');
let ReactDOM = require('react-dom');
import API from '../actions/ASApiActionCreators';
import CellStore from '../stores/ASCellStore';

/*
Image props have important  metadata about the size and position of an image. The offsets are computed relative to
the location of the original placement of the image, based purely on cell location. For example, if an image is
dragged a bunch to the right, its X offset would be a large number, and it'd have no Y offset.
Note that when rendering, the "top" style attribute adds the offset to the original top placement.
*/

/*
NOTE: This component was modified to scroll properly with the grid, but it uses setState and is slower than the canvas scrolling.
A better solution might be to use canvas to drag and resize and image, and repaint faster. Also, I couldn't figure out the CSS
for not scrolling over the column and row headers (what we want is a restricted overflow=hidden in ASSpreadsheet; this is what
currently stops the images from going up too high) -- Ritesh 12/16
*/

export default React.createClass({

  propTypes: {
    scrollPixels: React.PropTypes.object.isRequired,
    overlay: React.PropTypes.object.isRequired
  },

  /* Upon drag or resize, update the backend metadata about the image */
  updateImageProps(prop) {
    let rng = {tl: this.props.overlay.loc.index, br: this.props.overlay.loc.index};
    API.setProp(prop, rng);
  },

  /* Called when we stop dragging the image. We produce the new image props (by adding to offsets) and update backend */
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

  /*
  This is called after resizing and dragging, but will only do something nontrivial for resizing. It will get
  the new node, and compute the new prop based on the new width/height. This will then be sent to backend as an update.
  */
  _onMouseUp(e) {
    let node = ReactDOM.findDOMNode(this.refs.overlay);
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
    /*
    Compute div style based on metadata, allow for resizing, have a nice border
    We are passed scroll state from the spreadsheet (how much the sheet is scrolled in X and Y directions, in pixels)
    We account for that in computing top and left, in addition to keeping track of offsets (from user dragging)
    */
    let baseStyle = {
      position: 'absolute',
      width: this.props.overlay.width,
      height: this.props.overlay.height,
      top: this.props.overlay.top + this.props.overlay.offsetY - parseFloat(this.props.scrollPixels.y),
      left: this.props.overlay.left + this.props.overlay.offsetX - parseFloat(this.props.scrollPixels.x),
      zIndex: 5,
      resize: 'auto',
      overflow: 'hidden',
      border: "solid 2px black",
    };

    /*
    Render something if the image should be in view, as dictated by the props. Unfortunately, onMouseUp doesn't seem to
    work within Draggable, so it's in a separate div
    */
    return (
      <div onMouseUp={this._onMouseUp}>
        <Draggable
          moveOnStartChange={false}
          start={{x: 0, y: 0}}
          zIndex={100}
          onStop={this._onStop}>
          <div style={baseStyle} ref="overlay" >
            {this.props.overlay.elem}
          </div>
        </Draggable>
      </div>
    );

  }

});
