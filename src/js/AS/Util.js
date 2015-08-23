import Constants from '../Constants';

/* This module has general utility functions */

export default {

/*************************************************************************************************************************/
// Cell rendering

  /* Used to know what to display on the sheet */
  showValue(cv) {
    console.log("In show value: " + JSON.stringify(cv));
    let self = this;
    switch (cv.tag) {
      case "ValueNaN":
        return "NaN";
      case "ValueB":
        return cv.contents;
      case "ValueD":
        return cv.contents;
      case "ValueI":
        return cv.contents;
      case "ValueS":
        return cv.contents;
      case "ValueL":
        console.log("FOUND LIST");
        return self.showValue(cv.contents[0]);
      case "ValueError":
        return "ERROR";
      case "DisplayValue":
        return cv.displayValue;
    }
  },

  parseTagIntoRenderConfig(tag) {
    let self = this;
    switch(tag.tag) {
      case "TextColor":
        config.fgColor = self.colorToHtml(tag.contents);
        return config;
      case "BgColor":
        config.bgColor = self.colorToHtml(tag.contents);
        return config;
      case "Align":
        config.halign = tag.contents.toLowerCase();
        return config;
      case "Money":
        config.value = self.formatMoney(config.value, tag.contents, 2);
        return config;
      case "Percentage":
        config.value = self.formatPercentage(config.value);
        return config;
    }
  },

/*************************************************************************************************************************/
// Formatting

  formatMoney(currency, contents, dec) {
    let delim = null,
        val = contents.toString();
    switch(currency) {
      case "$":
        delim = ".";
        break;
      default:
        delim = ",";
        break;
    }
    return currency + val.substring(0,2) + delim + val.substring(2,2+dec);
  },

  formatPercentage(contents) {
    if (contents >= 0 && contents <= 100) {
      return contents + "%";
    } // TODO raise error otherwise
  },

/*************************************************************************************************************************/
// Misc

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
