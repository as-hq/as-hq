/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASSelectionObject
} from '../../types/Eval';

import type {
  ASOverlaySpec
} from '../../types/Hypergrid';

import Constants from '../../Constants';

let LocationUtils = {
    // Check if the mouse location is in the square box for draggging
  mouseLocIsContainedInBox(
    mouseLocX: number,
    mouseLocY: number,
    topLeftBoxObj: ?HGPoint,
    boxWidth: number
  ): boolean {
    if (topLeftBoxObj == null) {
      return false;
    }
    let xInBounds = mouseLocX >= topLeftBoxObj.x &&
                    mouseLocX <= topLeftBoxObj.x + boxWidth,
        yInBounds = mouseLocY >= topLeftBoxObj.y &&
                    mouseLocY <= topLeftBoxObj.y + boxWidth;
    return xInBounds && yInBounds;
  }
};

export default LocationUtils;
