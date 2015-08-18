import Constants from '../Constants';

function showValue(cv) {
  switch (cv.tag) {
    case "ValueNaN":
      // console.log("got undefined");
      return "undefined";
    case "ValueB":
    case "ValueD":
    case "ValueI":
    case "ValueS":
    return cv.contents;
    case "ValueL":
    return showValue(cv.contents[0]);
    case "ValueError":
    return "ERROR";
    case "StyledValue":
    return showValue(cv.value);
    case "DisplayValue":
    return cv.displayValue;
  }
};

export default {
  showValue: showValue,

  arrContains(arr, elem) {
    return arr.indexOf(elem) > -1;
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

  intToChar(i){
    return 'ABCDEFGHIJKLMNOPQRSTUVXYZ'.charAt(i);
  },

  getOrientedCorners(rng) {
    var tl = {row: Math.min(rng.row,rng.row2), col: Math.min(rng.col,rng.col2)},
        br = {row: Math.max(rng.row,rng.row2), col: Math.max(rng.col,rng.col2)};
    return {tl: tl, br: br};
  },

  intToExcelCol(i){
    // TODO only double letters supported
    let c = i % 26;
    let n = Math.floor(i / 26);
    if (n <= 26)
      return this.intToChar(n) + this.intToChar(c);
  },

  locToExcel(loc) {
    if (loc.row2){
      let {tl, br} = this.getOrientedCorners(loc);
      return this.intToExcelCol(tl.col) + tl.row
        + ":"
        + this.intToExcelCol(br.col) + br.row;
    } else {
      return this.intToExcelCol(loc.col) + loc.row;
    }
  },

  removeLastWord(str){
    let lastIndex = str.lastIndexOf(" ");
    if (lastIndex > 0)
      return str.substring(0, lastIndex);
    else return "";
  }
};
