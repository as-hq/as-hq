/* @flow */

import type {
  ASCellProp
} from '../types/Eval';

import type {
  ASOverlaySpec
} from '../types/Hypergrid';

import React from 'react';
import ReactDOM from 'react-dom';
import {Paper} from 'material-ui';
// $FlowFixMe too lazy to declare this import rn
import Draggable from 'react-draggable';
// $FlowFixMe too lazy to declare this import rn
import {Resizable} from 'react-resizable';

import Constants from '../Constants';
import CellStore from '../stores/ASCellStore';
import API from '../actions/ASApiActionCreators';
import {
  overlayOuter as overlayOuterZIndex,
  overlayInner as overlayInnerZIndex,
} from '../styles/zIndex';

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

type ASOverlayProps = {
  scrollPixels: HGPoint;
  overlay: ASOverlaySpec;
};

type ASOverlayState = {
  resizing: boolean,
  width: number,
  height: number
};

type EventDetail = {
  position: {top: string, left: string}
};

export default class ASOverlay extends React.Component<{}, ASOverlayProps, ASOverlayState> {

  constructor(props: ASOverlayProps) {
    super(props);

    let {overlay: {initWidth, initHeight}} = props;
    this.state = {
      resizing: false,
      width: initWidth,
      height: initHeight
    };
  }

  /* Upon drag or resize, update the backend metadata about the image */
  updateImageProps(prop: ASCellProp) {
    if (this.props.overlay.loc) {
      API.setProp(prop, this.props.overlay.loc.toRange());
    }
  }

  /* Called when we stop dragging the image. We produce the new image props (by adding to offsets) and update backend */
  _onDragStop(e: SyntheticEvent, {position}: EventDetail) {
    // ImageData shouldn't leak into this part of the code; should probably
    // create an ImageData type with imageOffsetX, etc., and pass that around,
    // and only insert tag: "ImageData" in the API. (Alex 12/15)
    let {overlay: {offsetX, offsetY}} = this.props;
    let {width, height} = this.state;
    let prop = {
      tag: "ImageData",
      imageOffsetX: parseFloat(position.left) + offsetX,
      imageOffsetY: parseFloat(position.top) + offsetY,
      imageWidth: width,
      imageHeight: height
    };

    if (position.left !== 0 || position.top !== 0) {
      this.updateImageProps(prop);
    }
  }

  _onResizeStart(e: SyntheticEvent, size: any) {
    this.setState({resizing: true});
  }

  _onResizeStop(e: SyntheticEvent, {size: {width, height}}: any) {
    this.setState({resizing: false});
    let {overlay: {offsetX, offsetY}} = this.props;
    let tagValue = {
      tag: "ImageData",
      imageOffsetX: offsetX,
      imageOffsetY: offsetY,
      imageWidth: width,
      imageHeight: height
    };
    this.updateImageProps(tagValue);
  }

  _onResize(e: SyntheticEvent, {size: {width, height}}: any) {
    this.setState({width: width, height: height});
  }

  _shouldUpdateDrag(e: SyntheticEvent, coreEvent: any): boolean {
    return !this.state.resizing;
  }

  render(): React.Element {
    /*
    Compute div style based on metadata, allow for resizing, have a nice border
    We are passed scroll state from the spreadsheet (how much the sheet is scrolled in X and Y directions, in pixels)
    We account for that in computing top and left, in addition to keeping track of offsets (from user dragging)
    */
    let {overlay, scrollPixels} = this.props;
    let {top, left, offsetX, offsetY} = overlay;
    let {width, height} = this.state;
    let baseStyle = {
      position: 'absolute',
      width: width,
      height: height,
      top: top + offsetY - scrollPixels.y,
      left: left + offsetX - scrollPixels.x,
      zIndex: overlayInnerZIndex,
      background: 'white'
    };

    return (
        <Draggable
          moveOnStartChange={false}
          start={{x: 0, y: 0}}
          zIndex={overlayOuterZIndex}
          onStop={this._onDragStop.bind(this)}
          onDrag={this._shouldUpdateDrag.bind(this)} >

            <div style={baseStyle} >
              <Resizable
                className="box"
                height={height}
                width={width}
                onResize={this._onResize.bind(this)}
                onResizeStart={this._onResizeStart.bind(this)}
                onResizeStop={this._onResizeStop.bind(this)} >
                  <span>{overlay.renderElem({width: width, height: height})}</span>
              </Resizable>
            </div>

        </Draggable>
    );

  }

}
