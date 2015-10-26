import Constants from '../Constants';
import Util from '../AS/Util';
import T from '../AS/Types';
import TC from '../AS/TypeConversions';

export default {

/**************************************************************************************************************************/
  /* External conversions */

  externalStringToExpression(str, lang) {
    if (lang.Server == "Excel") { // is language.Editor the correct thing?
      return str;
    } else if (str != null && typeof(str) != "undefined") {
      if (!isNaN(Number(str))) {
        return str;
      } else if (str.toUpperCase() == "TRUE") {
        return this.externalStringToBool(true, lang.Server);
      } else if (str.toUpperCase() == "FALSE") {
        return this.externalStringToBool(false, lang.Server);
      } else {
        return JSON.stringify(str);
      }
    } else {
      return ""; // unclear if we ever get here -- Alex 10/19
    }
  },

  externalStringToBool(b, lang) {
    if (b) {
      if (["R", "OCaml"].indexOf(lang) != -1) {
        return "true";
      } else {
        return "True";
      }
    } else {
      if (["R", "OCaml"].indexOf(lang) != -1) {
        return "false";
      } else {
        return "False";
      }
    }
    throw "Should never make it to the end of _dispBoolInLang";
  },

  _arrayToASCells(loc, language) {
    let self = this;
     return function(i){
       return function(v, j) {
        let asIndex = TC.simpleToASIndex({col: loc.col + j, row: loc.row + i}),
            xpObj = { expression: self.externalStringToExpression(v, language),
                      language: language} ;
         return TC.makeEvalCell(asIndex, xpObj);
       };
     };
   },

  _rowValuesToASCells(loc, language) {
    var self = this;
    return function(values, i){
      return values.map(self._arrayToASCells(loc, language)(i));
    };
  },

 // takes in a set of locations and the values at those locations,
  externalStringsToASCells(loc, strs, language) {
    return strs.map(this._rowValuesToASCells(loc, language));
  },

  extendRangeByCache(rng) {
    let tl = Util.getSafeIndex({row: rng.tl.row-Constants.scrollCacheY, col: rng.tl.col-Constants.scrollCacheX}),
        br = Util.getSafeIndex({row: rng.br.row+Constants.scrollCacheY, col: rng.br.col+Constants.scrollCacheX});
    return { tl: tl, br: br };
  }
}
