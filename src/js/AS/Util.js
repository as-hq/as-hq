import Constants from '../Constants';

export default {

  getLanguageFromEngine(eng) {
    switch(eng) {
      case Constants.Engines.Python:
        return 'python';
    }
  },

  arrContains(arr, elem) {
    return arr.indexOf(elem) > -1;
  },

  showValue(cv) {
    switch (cv.tag) {
      case "ValueNaN":
        console.log("got undefined");
        return "undefined";
      case "ValueB":
      case "ValueD":
      case "ValueI":
      case "ValueS":
      return cv.contents;
      case "ValueL":
      return this.showValue(cv.contents[0]);
      case "ValueError":
      return "ERROR";
      case "StyledValue":
      return this.showValue(cv.value);
      case "DisplayValue":
      return cv.displayValue;
    }
  },

  getIndicesOf(searchStr, str) {
    console.log("indexing string: "+str);
    let startIndex = 0, searchStrLen = searchStr.length;
    let index, indices = [];
    while ((index = str.indexOf(searchStr, startIndex)) > -1) {
        indices.push(index);
        startIndex = index + searchStrLen;
    }
    return indices;
  },

  toggleReferenceType(xp) {
    let dollarPresence = this.getIndicesOf("$", xp, true);
    let len = dollarPresence.length;
    if (len == 0)
      return "$" + xp;
    else if (len == 1) {
      if (dollarPresence[0] == 0){
        return xp.substring(1,2) + "$" + xp.substring(2);
      } else return "$" + xp;
    } else {
      return xp.replace(/\$/g, "");
    }
    return "ERROR";
  },

  locToExcel(loc) {
    if (loc.length == 2)
      return this.intToExcelCol(loc[1]) + (loc[0]+1);
    else{
      let corners = getOrientedCorners(loc),
          tl = corners[0],
          br = corners[1];
      return this.intToExcelCol(tl[1]) + (tl[0]+1)
        + ":"
        + this.intToExcelCol(br[1]) + (br[0]+1);
    }
  },

  intToExcelCol(i){
    let c = i % 26;
    let n = Math.floor(i / 26);
    if (n <= 26)
      return this.intToChar(n) + this.intToChar(c);
  },

  intToChar(i){
    return 'ABCDEFGHIJKLMNOPQRSTUVXYZ'.charAt(i);
  }
};
