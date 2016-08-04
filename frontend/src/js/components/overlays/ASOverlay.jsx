/* @flow */

import type {
  ASCellProp
} from '../../types/Eval';

import type {
  ASOverlaySpec,
  DragEventDetail,
  ResizeEventDetail
} from '../../types/Overlay';

import React from 'react';
import ReactDOM from 'react-dom';
// $FlowFixMe 
import Draggable from 'react-draggable';
// $FlowFixMe 
import {Resizable} from 'react-resizable';

import {
  overlayOuter as overlayOuterZIndex,
  overlayInner as overlayInnerZIndex,
} from '../../styles/zIndex';

type ASOverlayProps = {
  overlay: ASOverlaySpec;
  topLeft: {top: number, left: number};
  onDragStop: (o: ASOverlaySpec, d: DragEventDetail) => void;
  onResizeStop: (o: ASOverlaySpec, r: ResizeEventDetail) => void;
  onResizeStart: () => void;
  onDrag: () => boolean;
  onResize: (o: ASOverlaySpec, r: ResizeEventDetail) => void;
  onKeyDown: (o: ASOverlaySpec, e: SyntheticKeyboardEvent) => void;
};

// TODO: implement shouldComponentUpdate after OverlaySpecs are immutable

// Using pure component syntax
const ASOverlay = (props: ASOverlayProps) => {
  let overlay = props.overlay,
      {top, left} = props.topLeft,
      // left and top come from Hypergrid, and take scrolling and orig cell position into account
      // dragOffsetLeft = cumulative sum of drag movements by user to left, add to get the correct
      // left offset from the top left of the grid
      translateStr = 
        `translate(${left + overlay.dragOffsetLeft}px, ${top + overlay.dragOffsetTop}px)`,
      baseStyle = {
        position: 'absolute',
        display: 'block',
        width: overlay.imageWidth,
        height: overlay.imageHeight,
        zIndex: overlayInnerZIndex,
        transform: translateStr,
        background: 'white'
      };
  // We want an overlay to be draggable and resizable
  // The span between Draggable and Resizable only exists because Draggable injects its own
  // CSS transform, but we're using one as well (because it has about 6x painting speed compared
  // to setting top and left in CSS). It is thus a "buffer element".
  const resizeable = 
    <Resizable
      height={overlay.imageHeight}
      width={overlay.imageWidth}
      onResize={(e, r) => props.onResize(overlay, r)}
      onResizeStart={() => props.onResizeStart()}
      onResizeStop={(e, r) => props.onResizeStop(overlay, r)} >
          <div tabIndex="-1" 
               style={baseStyle} 
               onKeyDown={(e) => props.onKeyDown(overlay, e)}>
          {overlay.renderElem({
            width: overlay.imageWidth, height: overlay.imageHeight})}
          </div>
    </Resizable>;

  if (overlay.shouldDrag) {
    return (
      <Draggable
        zIndex={overlayOuterZIndex}
        onStop={(e, d) => props.onDragStop(overlay, d)}
        onDrag={() => props.onDrag()} >
          <span style={{
            display: 'block',
            zIndex: overlayOuterZIndex}} >
          {resizeable}
          </span>
      </Draggable>
    );
  } else {
    return resizeable;
  }

}

export default ASOverlay;
