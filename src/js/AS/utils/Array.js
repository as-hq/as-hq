/* @flow */

import _ from 'lodash';

let ArrayUtils = {
  sliceArray<T>(begin: number, end: number): ((arr: Array<T>) => Array<T>) {
    return (arr) => arr.slice(begin, end);
  },

  makeArrayOf<T>(value: T, length: number): Array<T> {
    return _.range(length).map(() => value);
  },

  make2DArrayOf<T>(value: T, height: number, length: number): Array<Array<T>> {
    return ArrayUtils.makeArrayOf(
      ArrayUtils.makeArrayOf(value, length),
      height
    );
  },

  concatAll<T>(arrs: Array<Array<T>>): Array<T> {
    return [].concat.apply([], arrs);
  },

  removeEmpty<T>(arr: Array<?T>): Array<T> {
    let ret: Array<T> = [];
    arr.forEach((ele) => {
      if (ele != null) {
        ret.push(ele);
      }
    });

    return ret;
  }
};

export default ArrayUtils;
