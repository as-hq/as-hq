/* @flow */

import ASCell from '../classes/ASCell';
import ASIndex from '../classes/ASIndex';

import type {
  ASOverlaySpec
} from '../types/Overlay';

import type {
  ASAction
} from '../types/Actions';

import React from 'react';
import {fromJS, Map, Record, Record$Class} from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import U from '../AS/Util';

// TODO: make the very definitions of ASIndex and OverlaySpec immutable so that no from/toJS is necessary

type ASOverlayState = any;

type CellStoreDataFields = {
  allCells: ASCellGrid;
  allErrors: Array<ASClientError>;
};

type ASOverlayState = Record$Class;

const ASOverlayStateRecord = Record({
  cellOverlays: Map(),
  floatingOverlays: Map(),
});

class ASOverlayStore extends ReduceStore<ASOverlayState> {

  // We store the overlays that are cell-bound and those that aren't in maps.
  getInitialState(): ASOverlayState {
    return new ASOverlayStateRecord();
  }

  reduce(state: ASOverlayState, action: ASAction): ASOverlayState {
    switch (action._type) {
      case 'RESET':
        _data.overlays = {};
        break;
      case 'ADD_OVERLAY_WITHOUT_LOC':
        let uid = U.Render.getUniqueId(),
            overlay = action.overlay;
        return this._setFloatingOverlay(state, overlay);
      case 'SCROLLED':
        // Need to re-position overlays
        this.__emitChange();
        return state;
      // We listen to cell updates to see if we need to add/change any overlays
      case 'GOT_UPDATED_CELLS':
        let cells = action.newCells;
        let newState = state;
        cells.forEach((cell) => {
          // Delete any existing overlays at the cell location, so that if the cell updates
          // the overlay will also update
          newState = this._delLoc(newState, cell.location)
          let possibleOverlay = this.getPossibleOverlay(cell);
          // If we have an image cell, update state, and the new overlay will render
          if (possibleOverlay != null) {
            newState = this._setCellOverlay(newState, possibleOverlay)
          }
        });
        return newState;
      case 'OVERLAY_RESIZED':
        let newOverlay = action.overlay;
        newOverlay.imageWidth = action.width;
        newOverlay.imageHeight = action.height;
        // Modify the width and height of the overlay, and set in state
        if (newOverlay.loc !== null) {
          return this._setCellOverlay(state, newOverlay);
        } else {
          return this._setFloatingOverlay(state, newOverlay);
        }
      case 'OVERLAY_DELETED':
        let delOverlay = action.overlay;
        // Delete this overlay from state, using either id or loc depending on cell-bound
        if (delOverlay.loc !== null) {
          return this._delCellOverlay(state, delOverlay);
        } else {
          return this._delFloatingOverlay(state, delOverlay);
        }
      default:
        return state;
    }
  }

  _setCellOverlay(state: ASOverlayState, overlay: ASOverlaySpec): ASOverlayState {
    const loc = fromJS(overlay.loc);
    const o = fromJS(overlay);
    return state.update('cellOverlays', (owl) => owl.set(loc, o))
  }

  _delCellOverlay(state: ASOverlayState, overlay: ASOverlaySpec): ASOverlayState {
    return this._delLoc(state, overlay.loc);
  }

  _setFloatingOverlay(state: ASOverlayState, overlay: ASOverlaySpec): ASOverlayState {
    const id = fromJS(overlay.id);
    const o = fromJS(overlay);
    return state.update('floatingOverlays', (owl) => owl.set(id, o));
  }

  _delFloatingOverlay(state: ASOverlayState, overlay: ASOverlaySpec): ASOverlayState {
    return state.update('floatingOverlays', (owl) => owl.delete(fromJS(overlay.id)));
  }

  _delLoc(state: ASOverlayState, loc: ASIndex): ASOverlayState {
    return state.update('cellOverlays', (owl) => owl.delete(fromJS(loc)));
  }

  // Sees if an ASCell is an image cell, and produces the ASOverlay object if so.
  // Initialize default sizes and positions.
  getPossibleOverlay(cell: ASCell): ?ASOverlaySpec {
    if (!cell.isImage()) {
      return null;
    }
    let imageSrc = Constants.getBackendUrl('http',
      Constants.BACKEND_STATIC_PORT) + "/images/" + cell.value.imagePath;

    // Initialize prop values, then see if the cell has data that overrides them
    let imageWidth = 300, imageHeight = 300, dragOffsetTop = 0, dragOffsetLeft = 0;
    cell.props.forEach((prop) => {
      if (prop.tag === 'ImageData') {
        imageWidth = prop.imageWidth;
        imageHeight = prop.imageHeight;
        dragOffsetTop = prop.dragOffsetTop;
        dragOffsetLeft = prop.dragOffsetLeft;
      }
    });

    return {
      id: U.Render.getUniqueId(),
      renderElem: (style) => {
        return (
          <image src={imageSrc} draggable="false" style={style} alt="Error rendering image." />
        );
      },
      imageWidth,
      imageHeight,
      dragOffsetTop,
      dragOffsetLeft,
      loc: cell.location
    };
  }

  // Loop over state to get all the overlays
  getAllOverlays(): Array<ASOverlaySpec> {
    let overlays = [];
    for (let overlay of this.getState().get('cellOverlays').values()) {
      overlays.push(overlay.toObject());
    }
    for (let overlay of this.getState().get('floatingOverlays').values()) {
      overlays.push(overlay.toObject());
    }
    return overlays;
  }

}

export default new ASOverlayStore(Dispatcher);
