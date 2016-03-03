/* @flow */

import type {
  ASLocation,
  ASLocationObject
} from '../../types/Eval';

import Constants from '../../Constants';

import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';

let LocationUtils = {
  asLocsToASIndices(locs: Array<ASLocation>): Array<ASIndex> {
    let ret = [];
    locs.forEach((loc) => {
      if (loc instanceof ASIndex) {
        ret.push(loc);
      }
    });

    return ret;
  },

  makeLocations(locObjs: Array<ASLocationObject>): Array<ASLocation> {
    let ret: Array<ASLocation> = [];

    locObjs.forEach((locObj) => {
      if (locObj.tag === 'index') {
        ret.push(new ASIndex(locObj));
      } else if (locObj.tag === 'range') {
        ret.push(new ASRange(locObj));
      }
    });

    return ret;
  },

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
