import Constants from '../Constants';
import shortid from 'shortid';

/* This module has general utility functions */

var colors = {"aliceblue":"#f0f8ff","antiquewhite":"#faebd7","aqua":"#00ffff","aquamarine":"#7fffd4","azure":"#f0ffff",
      "beige":"#f5f5dc","bisque":"#ffe4c4","black":"#000000","blanchedalmond":"#ffebcd","blue":"#0000ff","blueviolet":"#8a2be2","brown":"#a52a2a","burlywood":"#deb887",
      "cadetblue":"#5f9ea0","chartreuse":"#7fff00","chocolate":"#d2691e","coral":"#ff7f50","cornflowerblue":"#6495ed","cornsilk":"#fff8dc","crimson":"#dc143c","cyan":"#00ffff",
      "darkblue":"#00008b","darkcyan":"#008b8b","darkgoldenrod":"#b8860b","darkgray":"#a9a9a9","darkgreen":"#006400","darkkhaki":"#bdb76b","darkmagenta":"#8b008b","darkolivegreen":"#556b2f",
      "darkorange":"#ff8c00","darkorchid":"#9932cc","darkred":"#8b0000","darksalmon":"#e9967a","darkseagreen":"#8fbc8f","darkslateblue":"#483d8b","darkslategray":"#2f4f4f","darkturquoise":"#00ced1",
      "darkviolet":"#9400d3","deeppink":"#ff1493","deepskyblue":"#00bfff","dimgray":"#696969","dodgerblue":"#1e90ff",
      "firebrick":"#b22222","floralwhite":"#fffaf0","forestgreen":"#228b22","fuchsia":"#ff00ff",
      "gainsboro":"#dcdcdc","ghostwhite":"#f8f8ff","gold":"#ffd700","goldenrod":"#daa520","gray":"#808080","green":"#008000","greenyellow":"#adff2f",
      "honeydew":"#f0fff0","hotpink":"#ff69b4", "indianred ":"#cd5c5c","indigo":"#4b0082","ivory":"#fffff0","khaki":"#f0e68c",
      "lavender":"#e6e6fa","lavenderblush":"#fff0f5","lawngreen":"#7cfc00","lemonchiffon":"#fffacd","lightblue":"#add8e6","lightcoral":"#f08080","lightcyan":"#e0ffff","lightgoldenrodyellow":"#fafad2",
      "lightgrey":"#d3d3d3","lightgreen":"#90ee90","lightpink":"#ffb6c1","lightsalmon":"#ffa07a","lightseagreen":"#20b2aa","lightskyblue":"#87cefa","lightslategray":"#778899","lightsteelblue":"#b0c4de",
      "lightyellow":"#ffffe0","lime":"#00ff00","limegreen":"#32cd32","linen":"#faf0e6",
      "magenta":"#ff00ff","maroon":"#800000","mediumaquamarine":"#66cdaa","mediumblue":"#0000cd","mediumorchid":"#ba55d3","mediumpurple":"#9370d8","mediumseagreen":"#3cb371","mediumslateblue":"#7b68ee",
      "mediumspringgreen":"#00fa9a","mediumturquoise":"#48d1cc","mediumvioletred":"#c71585","midnightblue":"#191970","mintcream":"#f5fffa","mistyrose":"#ffe4e1","moccasin":"#ffe4b5",
      "navajowhite":"#ffdead","navy":"#000080","oldlace":"#fdf5e6","olive":"#808000","olivedrab":"#6b8e23","orange":"#ffa500","orangered":"#ff4500","orchid":"#da70d6",
      "palegoldenrod":"#eee8aa","palegreen":"#98fb98","paleturquoise":"#afeeee","palevioletred":"#d87093","papayawhip":"#ffefd5","peachpuff":"#ffdab9","peru":"#cd853f","pink":"#ffc0cb","plum":"#dda0dd","powderblue":"#b0e0e6","purple":"#800080",
      "red":"#ff0000","rosybrown":"#bc8f8f","royalblue":"#4169e1","saddlebrown":"#8b4513","salmon":"#fa8072","sandybrown":"#f4a460","seagreen":"#2e8b57","seashell":"#fff5ee","sienna":"#a0522d","silver":"#c0c0c0","skyblue":"#87ceeb","slateblue":"#6a5acd","slategray":"#708090","snow":"#fffafa","springgreen":"#00ff7f","steelblue":"#4682b4",
      "tan":"#d2b48c","teal":"#008080","thistle":"#d8bfd8","tomato":"#ff6347","turquoise":"#40e0d0",
      "violet":"#ee82ee","wheat":"#f5deb3","white":"#ffffff","whitesmoke":"#f5f5f5","yellow":"#ffff00","yellowgreen":"#9acd32"};

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
        return self.showValue(cv.contents[0]);
      case "ValueError":
        return "ERROR";
      case "ValueImage":
        return "IMAGE";
      case "DisplayValue":
        return cv.displayValue;
      default:
        return JSON.stringify(cv.contents);
    }
  },

  parseTagIntoRenderConfig(config, tag) {
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
      case "Streaming":
        config.isStreaming = true;
        return config;
    }
  },

  getBorderPatternsForInteriorCell(col, row, rng) {
    if (col >= rng.col && col <= rng.col2 && row >= rng.row && row <= rng.row2){
      let borders = [];
      if (col === rng.col) // left intersection
        borders.push([[0,0],[0,1]]);
      if (col === rng.col2) // right intersection
        borders.push([[1,0],[1,1]]);
      if (row === rng.row) // top intersection
        borders.push([[0,0],[1,0]]);
      if (row === rng.row2) // bottom intersection
        borders.push([[0,1],[1,1]]);
      return borders;
    } else return [];
  },

// determines borders of a cell to be painted, given that it falls somewhere within a list of locs
// returns a list of edges that can be painted in any order
// each edge is a 2-length array [start, end]
// executed by graphicscontext.moveTo(startx, starty) -> graphicscontext.lineTo(endx, endy)
  getPaintedBorders(col, row, locs) {
    if (locs.constructor === Array) {
      // first try indices
      for (var i=0; i<locs.length; i++) {
        if (!locs[i].row2 && (col === locs[i].col && row === locs[i].row))
          return [[[0,0],[1,0]],
                  [[1,0],[1,1]],
                  [[1,1],[0,1]],
                  [[0,1],[0,0]]];
      }
      // then try interiors of ranges
      for (var i=0; i<locs.length; i++) {
        if (locs[i].row2)
          return this.getBorderPatternsForInteriorCell(col, row, locs[i]);
      }
      return [];
    }
    else if (locs.row2)
      return this.getBorderPatternsForInteriorCell(col, row, locs);
    else if (col === locs.col && row === locs.row)
      return [[[0,0],[1,0]],
              [[1,0],[1,1]],
              [[1,1],[0,1]],
              [[0,1],[0,0]]];
    else return [];
  },

  getOverlay(cv, col, row) {
    let self = this;
    switch(cv.tag) {
      case "ValueImage":
        return {
          tag: cv.tag,
          id: self.getUniqueId(),
          src: cv.imagePath,
          width: cv.imageWidth.toString(),
          height: cv.imageHeight.toString(),
          col: col,
          row: row
        };
      default:
        return null;
    }
  },

/*************************************************************************************************************************/
// Formatting

  formatMoney(currency, contents, dec) {
    let delim = null,
        sign = null,
        val = contents.toString();
    switch(currency) {
      case "USD":
        delim = ".";
        sign = "$";
        break;
      case "GBP":
        delim = ",";
        sign = "£";
      case "EUR":
        delim = ",";
        sign = "€";
      default:
        delim = "";
        sign = "";
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

  getUniqueId() {
    return shortid.generate();
  },

  colorToHtml(str) {
    if (str.charAt(0) === "#" || str.substring(0,2) === "rgb") // if color already correct format
      return str;
    else return this.colorNameToHex(str);
  },

  colorNameToHex(color) {
    if (typeof colors[color.toLowerCase()] != 'undefined')
        return colors[color.toLowerCase()];
    return false;
  },

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
    // TODO generalize to arbitrary range lengths
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

  charToInt(c) {
    return c.charCodeAt(0) - 64;
  },

  getOrientedCorners(rng) {
    var tl = {row: Math.min(rng.row,rng.row2), col: Math.min(rng.col,rng.col2)},
        br = {row: Math.max(rng.row,rng.row2), col: Math.max(rng.col,rng.col2)};
    return {tl: tl, br: br};
  },

  getOrientedArea(rng) {
    return {
      row: Math.min(rng.row, rng.row2), col: Math.min(rng.col, rng.col2),
      row2: Math.max(rng.row, rng.row2), col2: Math.max(rng.col, rng.col2)
    };
  },

  excelToIdx(xp) {
    var row=0, col=0, i=0, charIdx = 0;
    while(i < xp.length && isNaN(xp.charAt(i))){
      charIdx = i+1;
      i++;
    }
    var rawCol = xp.substring(0, charIdx), rawRow = xp.substring(charIdx);
    for (var c=0; c<charIdx; c++) {
      col = col + this.charToInt(xp.charAt(c)) * Math.pow(26, charIdx - c-1);
    }

    return {col: col, row: parseInt(rawRow)};
  },

  excelToLoc(xp) {
    let endpoints = xp.split(":");
    if (endpoints.length === 1)
      return this.excelToIdx(endpoints[0]);
    else {
      let start = this.excelToIdx(endpoints[0]),
          end = this.excelToIdx(endpoints[1]);
      return {row: start.row, col: start.col, row2: end.row, col2: end.col};
    }
  },

  intToExcelCol(i) {
    var quo = Math.floor((i) / 26);
    var rem = (i) % 26;
    var code = '';
    if (quo > 0) {
        code += String.fromCharCode('A'.charCodeAt(0) + quo - 1);
    }
    code += String.fromCharCode('A'.charCodeAt(0) + rem);
    return code;
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
  },

  parseDependencies(str) {
    console.log("parsing dependencies of: " + str);
    if (str === "")
      return [];
    else{
      let regIdx = /[A-Z]+[0-9]+/g, regRng = /[A-Z]+[0-9]+:[A-Z]+[0-9]+/g;
      let rngs = str.match(regRng);
      let idxStr = str.replace(regRng, "");
      let idxs = idxStr.match(regIdx);
      let matches = null;
      if (rngs && idxs)
        matches = rngs.concat(idxs);
      else if (rngs)
        matches = rngs;
      else if (idxs)
        matches = idxs;
      else
        matches = [];
      console.log("parsed deps: "+JSON.stringify(matches));
      // return matches.map(this.excelToLoc);
      let parsed = [];
      for (var i=0; i<matches.length; i++)
        parsed.push(this.excelToLoc(matches[i]));
      // console.log(JSON.stringify(parsed));
      return parsed;
    }
  },

  // TODO handle sideways lists?
  getListDependency(headLocation, length) {
    return [{
      row: headLocation.row,
      col: headLocation.col,
      row2: headLocation.row + length - 1,
      col2: headLocation.col
    }];
  },

  isContainedInLoc(col, row, loc) {
    if (loc.row2)
      return (col >= loc.col && col <= loc.col2 && row >= loc.row && row <= loc.row2);
    else return (col === loc.col && row === loc.row);
  },

  isContainedInLocs(col, row, locs) {
    if (locs.constructor === Array) {
      for (var i=0; i<locs.length; i++) {
        if (this.isContainedInLoc(col, row, locs[i]))
          return true;
      }
      return false;
    }
    else return this.isContainedInLoc(col, row, locs);
  },

  getAgnosticLanguageFromServer(lang) {
    return Constants.Languages[lang];
  }
};
