/* @flow */

import type {
  ASSheet
} from '../../types/Eval';

let WorkbookUtils = {
  // merge arrays of sheet objects
  mergeSheets(arr1: Array<ASSheet>, arr2: Array<ASSheet>): Array<ASSheet> {
    for (var i=0; i<arr1.length; i++) {
      for (var j=0; i<arr2.length; j++) {
        if (arr1[i].sheetId === arr2[j].sheetId)
          arr1[i] = arr2[j];
        delete arr2[j];
      }
    }
    return arr1.concat(arr2);
  },

  // deletes elements from
  removeSheets(delFrom: Array<ASSheet>, del: Array<ASSheet>): Array<ASSheet> {
    for (var i=0; i<delFrom.length; i++) {
      for (var j=0; j<del.length; j++) {
        if (delFrom[i].sheetId === del[i].sheetId)
          delete delFrom[i];
      }
    }

    return delFrom;
  }
};

export default WorkbookUtils;
