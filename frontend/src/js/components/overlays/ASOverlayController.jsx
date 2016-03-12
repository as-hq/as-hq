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

import Constants from '../../Constants';
import OverlayActionCreator from '../../actions/ASOverlayActionCreators';
import OverlayStore from '../../stores/ASOverlayStore';
import API from '../../actions/ASApiActionCreators';
import U from '../../AS/Util';

import ASOverlay from './ASOverlay.jsx';

// We need a function with access to hg-positioning things
type ASOverlayControllerProps = {
  computeTopLeftPxOfLoc: (col: number, row: number) => {top: number, left: number};
};

// Keep track if any of the overlays is resizing. If so, then we shouldn't
// allow for dragging
type ASOverlayControllerState = {
  resizing: boolean;
};

export default class ASOverlayController extends React.Component {
  static defaultProps = {}; 
  props: ASOverlayControllerProps;
  state: ASOverlayControllerState;


  constructor(props: ASOverlayControllerProps) {
    super(props);
    this.state = {
      resizing: false
    };
  }

  // Listen to the OverlayStore and rerender upon update. Cannot use StoreLinks
  // for now because ReduceStore has addListener and not addChangeListener.
  componentDidMount() {
    OverlayStore.addListener(() => {
      this.forceUpdate();
    });
  }

  // If an overlay is a cell-bound overlay, then send an API message
  _setProp(prop: ASCellProp, overlay: ASOverlaySpec) {
    if (overlay.loc != null) {
      API.setProp(prop, overlay.loc.toRange());
    }
  }

  _dragIsNonTrivial(detail: DragEventDetail): boolean {
    return detail.position.top !== 0 || detail.position.left !== 0;
  }

  // Called when a drag of an overlay ends. If the drag actually moved the overlay,
  // send an update to backend, so that the position is correct on a reload.
  _onDragStop(overlay: ASOverlaySpec, detail: DragEventDetail) {
    if (this.state.resizing) {
      return;
    }
    let {dragOffsetLeft, dragOffsetTop, imageWidth, imageHeight} = overlay,
        prop = {
          tag: 'ImageData',
          dragOffsetTop: dragOffsetTop + parseFloat(detail.position.top),
          dragOffsetLeft: dragOffsetLeft + parseFloat(detail.position.left),
          imageWidth,
          imageHeight
        };
    if (this._dragIsNonTrivial(detail)) {
      this._setProp(prop, overlay);
    }
  }

  // Called when a resize stops. Update the overlay on backend so that it's correct
  // on a reload.
  _onResizeStop(overlay: ASOverlaySpec, detail: ResizeEventDetail) {
    this.setState({resizing: false});
    let {dragOffsetTop, dragOffsetLeft, imageWidth, imageHeight} = overlay,
        prop = {
          tag: 'ImageData',
          dragOffsetTop,
          dragOffsetLeft,
          imageWidth: detail.size.width,
          imageHeight: detail.size.height
        };
    this._setProp(prop, overlay);
  }

  // Given an overlay, account for scrolling and the cell's position.
  // The overlay's dragOffsetTop and dragOffsetLeft only account for the shifting due to the
  // dragging action. This function will account for Hypergrid scrolling and the initial location of the
  // cell that the overlay is bound to.
  _getTopLeftOfOverlay(overlay: ASOverlaySpec): {top: number, left: number} {
    if (overlay.loc != null) {
      return this.props.computeTopLeftPxOfLoc(overlay.loc.col, overlay.loc.row);
    } else {
      return {top: 0, left: 0};
    }
  }

  // When a non-trivial resize is occurring, fire a resize event, which will update the
  // overlays in the store and thus cause a rerender here. Don't need to tell backend
  // until the resize ends.
  _onResize(overlay: ASOverlaySpec, detail: ResizeEventDetail) {
    if (detail.size.width !== overlay.imageWidth ||
        detail.size.height !== overlay.imageHeight) {
      OverlayActionCreator.resize(overlay, detail.size.width, detail.size.height);
    }
  }

  // If the key pressed was a delete/backspace, fire a delete action, which will modify
  // the stores and cause an update here. Also, delete the original location of that overlay
  // if the overlay is cell-bound.
  _onKeyDown(overlay: ASOverlaySpec, e: SyntheticKeyboardEvent) {
    if (U.Key.isDestructiveKey(e)) {
      OverlayActionCreator.delete(overlay);
      if (overlay.loc != null) {
        API.deleteRange(overlay.loc.toRange());
      }
    }
  }

  // Just render all of the overlays from the OverlayStore, with accurate props.
  // In order to make sure that the overlay's don't go past the bars, you could
  // have a div over here with top: colHeight, left: rowWidth, width, height: 100%,
  // overflow: hidden, but this also requires updating this if those values are resized.
  // ^^ TODO
  // We're giving each overlay a key = uid for React's reconciliation
  // Just render all of the overlays from the store.
  render(): React.Element {
    const overlays = OverlayStore.getAllOverlays();
    return (
      <div style={{width: '100%', height: '100%', position: 'absolute'}} >
        {overlays.map((overlay) =>
          <ASOverlay
            key={overlay.id}
            overlay={overlay}
            topLeft={this._getTopLeftOfOverlay(overlay)}
            onKeyDown={(o, e) => this._onKeyDown(o, e)}
            onDrag={() => !this.state.resizing}
            onResize={(o, r) => this._onResize(o, r)}
            onResizeStart={() => this.setState({resizing: true})}
            onDragStop={(o, d) => this._onDragStop(o, d)}
            onResizeStop={(o, r) => this._onResizeStop(o, r)}/>
        )}
      </div>
    );
  }

}
