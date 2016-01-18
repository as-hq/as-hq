/* @flow */

/*
THIS IS NOT A DUMPING GROUND.
THIS IS FOR HELPER FUNCTIONS THAT WOULD BE UNDER THE SCOPE OF LODASH.
ALPHASHEETS TYPES (TYPES FROM TYPES FOLDER) SHOULD NEVER BE FOUND HERE.
 */

let GeneralUtils = {
  m(): any {
    let arr = Array.prototype.slice.call(arguments); //args not array, get array
    return arr.reduce((acc, cur) => cur ? Object.assign(acc, cur) : acc);
  },

  values<T>(o: any): any {
    return Object.keys(o).map((key) => o[key]);
  }
};

export default GeneralUtils;
