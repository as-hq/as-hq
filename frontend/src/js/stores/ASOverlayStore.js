/* @flow */

import ASCell from '../classes/ASCell';
import ASIndex from '../classes/ASIndex';

import type {
  ASClientError
} from '../types/Errors';

import type {
  ASLocationObject
} from '../types/Eval';

import type {
  ASOverlaySpec
} from '../types/Overlay';

import type {
  ASAction
} from '../types/Actions';

import type {
  ASCellGrid
} from '../types/State';

import type {
  CellUpdate
} from '../types/Updates';

import React from 'react';
import {fromJS, Map, Record, Record$Class} from 'immutable';
// $FlowFixMe
import { ReduceStore } from 'flux/utils';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import U from '../AS/Util';
import CellStore from './ASCellStore';

// TODO: make the very definitions of ASIndex and OverlaySpec immutable so that no from/toJS is necessary

type State = any;
const StateRecord = Record({
  cellOverlays: Map(),
  floatingOverlays: Map(),
});

class ASOverlayStore extends ReduceStore<State> {

  // We store the overlays that are cell-bound and those that aren't in maps.
  getInitialState(): State {
    return new StateRecord();
  }

  reduce(state: State, action: ASAction): State {
    switch (action._type) {
      case 'ADD_OVERLAY_WITHOUT_LOC': {
        const uid = U.Render.getUniqueId();
        const overlay = action.overlay;
        return setFloatingOverlay(state, overlay);
      }

      case 'SCROLLED': {
        // Need to re-position overlays
        this.__emitChange();
        return state;
      }

      case 'CLEARED_SHEET': {
        return new StateRecord();
      }

      case 'SHEET_UPDATED': {
        this.getDispatcher().waitFor([CellStore.getDispatchToken()]);
        return resetOverlays(state, action.update.cellUpdates);
      }

      case 'OVERLAY_RESIZED': {
        const newOverlay = action.overlay;
        newOverlay.imageWidth = action.width;
        newOverlay.imageHeight = action.height;

        // Modify the width and height of the overlay, and set in state
        if (newOverlay.loc !== null) {
          return setCellOverlay(state, newOverlay);
        } else {
          return setFloatingOverlay(state, newOverlay);
        }
      }

      case 'OVERLAY_DELETED': {
        const delOverlay = action.overlay;

        // Delete this overlay from state, using either id or loc depending on cell-bound
        if (delOverlay.loc !== null) {
          return delCellOverlay(state, delOverlay);
        } else {
          return delFloatingOverlay(state, delOverlay);
        }
      }

      default:
        return state;
    }
  }

  getPossibleOverlay(cell: ASCell): ?ASOverlaySpec {
    return getPossibleOverlay(cell);
  }

  // Loop over state to get all the overlays
  getAllOverlays(): Array<ASOverlaySpec> {
    const overlays = [];
    for (let overlay of this.getState().get('cellOverlays').values()) {
      overlays.push(overlay.toObject());
    }
    for (let overlay of this.getState().get('floatingOverlays').values()) {
      overlays.push(overlay.toObject());
    }
    return overlays;
  }

}

// Sees if an ASCell is an image cell, and produces the ASOverlay object if so.
// Initialize default sizes and positions.
function getPossibleOverlay(cell: ASCell): ?ASOverlaySpec {
  const {value} = cell;
  if (value.tag !== 'ValueImage') {
    return null;
  } else {
    const {imagePath} = value;
    const imageSrc = Constants.getBackendUrl(
      'http',
      Constants.BACKEND_STATIC_PORT)
      + "/images/" + imagePath;

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
}

function delLoc(state: State, loc: ASIndex): State {
  return state.update('cellOverlays', (owl) => owl.delete(fromJS(loc)));
}

function setCellOverlay(state: State, overlay: ASOverlaySpec): State {
  const loc = fromJS(overlay.loc);
  const o = fromJS(overlay);
  return state.update('cellOverlays', (owl) => owl.set(loc, o))
}

function delCellOverlay(state: State, overlay: ASOverlaySpec): State {
  const {loc} = overlay;

  if (loc === null || loc === undefined) {
    throw new Error('Undefined location not allowed in cell overlay');
  } else {
    return delLoc(state, loc);
  }
}

function setFloatingOverlay(state: State, overlay: ASOverlaySpec): State {
  const id = fromJS(overlay.id);
  const o = fromJS(overlay);
  return state.update('floatingOverlays', (owl) => owl.set(id, o));
}

function delFloatingOverlay(state: State, overlay: ASOverlaySpec): State {
  return state.update('floatingOverlays', (owl) => owl.delete(fromJS(overlay.id)));
}

function resetOverlays(state: State, update: CellUpdate): State {
  let state_ = state;

  ASCell.makeCells(update.newVals).forEach((cell) => {
    const possibleOverlay = getPossibleOverlay(cell);
    // If we have an image cell, update state, and the new overlay will render
    if (possibleOverlay != null) {
      state_ = setCellOverlay(state_, possibleOverlay)
    } else {
      state_ = delLoc(state_, cell.location);
    }
  });

  return state_;
}

export default new ASOverlayStore(Dispatcher);
