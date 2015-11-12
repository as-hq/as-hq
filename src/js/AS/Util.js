import {logDebug} from './Logger';

import Constants from '../Constants';
import {HOST_IP, HOST_WS_PORT} from '../Constants';
import shortid from 'shortid';
import T from './Types';


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

  getHostUrl() {
    // let baseUrl = process.env.NODE_ENV ? HOST_IP : 'localhost';
    // logDebug("GOT ENV ARG: ", process.env.NODE_ENV);
    // return 'ws://' + baseUrl + ':' + HOST_WS_PORT;
    return Constants.isRemote ? ('ws://' + HOST_IP + ':' + HOST_WS_PORT) : Constants.HOST_WS_URL;
  },

/*************************************************************************************************************************/
// Cell rendering

  /* Used to know what to display on the sheet */
  showValue(cv, isRepl) {
    // logDebug("In show value: " + JSON.stringify(cv));
    let self = this;
    switch (cv.tag) {
      case "NoValue":
        return "";
      case "ValueNaN":
        return "NaN";
      case "ValueB":
        return cv.contents.toString().toUpperCase();
      case "ValueD":
        return cv.contents;
      case "ValueI":
        return cv.contents;
      case "ValueS":
        return cv.contents;
      case "ValueL":
        if (isRepl)
          return self.showFullValueList(cv);
        else return self.showValue(cv.contents[0]);
      case "RList":
        if (isRepl)
          return JSON.stringify(cv.contents);
        else return "R_LIST";
      case "ValueError":
        if (isRepl)
          return cv.errMsg;
        else return "ERROR"; // TODO: show more descriptive errors. (#REF? #NAME?)
      case "ValueExcelError":
        return "ERROR";
      case "ValueImage":
        return "IMAGE";
      case "ValueObject":
        return cv.displayValue;
      case "DisplayValue":
        return cv.displayValue;
      default:
        logDebug("CELL CONTENTS SHOW VALUE: ", cv.contents);
        return JSON.stringify(cv.contents);
    }
  },

  showFullValueList(cv) {
    return JSON.stringify(cv.contents.map(this.showValue));
  },

  tagsToRenderConfig(config, tags) {
    let self = this;
    for (var i=0; i<tags.length; i++) {
      let tag = tags[i];
      switch(tag.tag) {
        case "TextColor":
          config.fgColor = self.colorToHtml(tag.contents);
          break;
        case "Bold":
          config.font = "bold " + config.font;
          break;
        case "Italic":
          config.font = "italic " + config.font;
          break;
        case "BgColor":
          config.bgColor = self.colorToHtml(tag.contents);
          break;
        case "Align":
          config.halign = tag.contents.toLowerCase();
          break;
        case "Format":
          switch (tag.contents) { 
            case "Money": 
              config.value = self.formatMoney("$", config.value, 2);
              break; 
            case "Percentage":
              config.value = self.formatPercentage(config.value);
              break;
          }
          break;
        case "Streaming":
          config.isStreaming = true;
          break;
        case "ListMember":
          config.bgColor = colors["cornsilk"];
          break;
        case "DFMember":
          config.bgColor = colors["lavender"];
          break;
        default:
          break;
      }
    }
    return config;
  },

  valueToRenderConfig(config, val) {
    switch(val.tag) {
      case "ValueI":
      case "ValueD":
        config.halign = 'right';
        return config;
      case "ValueS":
        config.halign = 'left';
        return config;
      case "RList":
        config.bgColor = colors['lightcyan'];
        return config;
      default:
        config.halign = 'center';
        return config;
    }
  },

  getPaintedBordersForSingleCell() {
    return [[[0,0],[1,0]],
            [[1,0],[1,1]],
            [[1,1],[0,1]],
            [[0,1],[0,0]]];
  },

  getBordersForInteriorCell(col, row, rng) {
    let {tl, br} = rng;
    if (T.isIndex(rng) && (col === tl.col && row === tl.row)) {
      return this.getPaintedBordersForSingleCell();
    } else {
      let borders = [null,null,null,null];
      if (col === tl.col) // left intersection
        borders[0] = [[0,0],[0,1]];
      if (col === br.col) // right intersection
        borders[1] = [[1,0],[1,1]];
      if (row === tl.row) // top intersection
        borders[2] = [[0,0],[1,0]];
      if (row === br.row) // bottom intersection
        borders[3] = [[0,1],[1,1]];
      return borders;
    }
  },

// determines borders of a cell to be painted, given that it falls somewhere within a list of locs
// returns a list of edges that can be painted in any order
// each edge is a 2-length array [start, end]
// executed by graphicscontext.moveTo(startx, starty) -> graphicscontext.lineTo(endx, endy)
  getPaintedBorders(col, row, rngs) {
    let result = rngs.map((rng) => this.getBordersForInteriorCell(col, row, rng), this);
    return this.concatAll(result);
  },

  getOverlay(cv, col, row) {
    let self = this;
    switch(cv.tag) {
      case "ValueImage":
        return {
          tag: cv.tag,
          id: self.getUniqueId(),
          src: Constants.HOST_STATIC_URL + "/images/" + cv.imagePath,
          width: "300",
          height: "300",
          col: col,
          row: row
        };
      default:
        return null;
    }
  },


  getX(col,scrollX){
    return (col-scrollX) * Constants.cellWidthPx + Constants.gridXOffset + "px";
  },

  getY(row,scrollY){
    return (row-scrollY)* Constants.cellHeightPx + Constants.gridYOffset + "px";
  },

/*************************************************************************************************************************/
// Formatting

  formatMoney(currency, contents, dec) {
    if (!contents || isNaN(contents) || contents == "") {
      return contents; 
    }

    let delim = null,
        sign = null,
        val = contents.toString();
    switch(currency) {
      case "$":
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
    let formatted = parseFloat(Math.round(contents * 100) / 100).toFixed(2), 
        decimalInd = formatted.length - 3; 
    formatted[decimalInd] = delim; 

    return sign + formatted;
  },

  formatPercentage(contents) {
    if (!contents || isNaN(contents) || contents == "") {
      return contents; 
    }
    return contents*100 + "%"; 
  },

/*************************************************************************************************************************/
// Misc

  // merge arrays of sheet objects
  mergeSheets(arr1, arr2) {
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
  removeSheets(delFrom, del) {
    for (var i=0; i<delFrom.length; i++) {
      for (var j=0; j<del.length; j++) {
        if (delFrom[i].sheetId === del[i].sheetId)
          delete delFrom[i];
      }
    }
  },

  isEmptyString(str) {
    return /\S/.test(str);
  },

  shiftIndex(ind, dr, dc) {
    return {row: ind.row + dr, col: ind.col + dc};
  },

  isEmptyCell(c) {
    return !c || ((c.cellExpression.expression == "") && (c.cellTags.length == 0));
  },

  removeEmptyLines(str){
    var lines = str.split("\n");
    var filtered = lines.filter(this.isEmptyString);
    return filtered.join("\n");
  },

  getIndicesOf(searchStr, str) {
    var startIndex = 0, searchStrLen = searchStr.length;
    var index, indices = [];
    while ((index = str.indexOf(searchStr, startIndex)) > -1) {
        indices.push(index);
        startIndex = index + searchStrLen;
    }
    return indices;
  },

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

  toggleReferenceType(xp) {
    // TODO generalize to arbitrary range lengths
    let deps = this.parseRefs(xp);
    if (deps.length === 0)
      return null
    else if (deps.length === 1)
      return this.toggleReferenceIndex(deps[0]);
    else throw "Single word contains multiple references.";
  },

  toggleReferenceIndex(ref) {
    let dollarIndices = this.getIndicesOf('$', ref),
        cleanRef = ref.replace(/\$/g, ''),
        row = cleanRef.split(/[A-Za-z]+/).pop(),
        col = cleanRef.substring(0, cleanRef.length - row.length);
    if (dollarIndices.length === 0) {
      return '$' + col + '$' + row;
    } else if (dollarIndices.length === 1 ) {
      if (dollarIndices[0] === 0)
        return col + row;
      else
        return '$' + col + row;
    } else if (dollarIndices.length === 2) {
      return col + '$' + row;
    } else return null;
  },

  intToChar(i){
    return 'ABCDEFGHIJKLMNOPQRSTUVXYZ'.charAt(i);
  },

  charToInt(c) {
    return c.charCodeAt(0) - 64;
  },

  orientRange(rng) {
    var tl = {row: Math.min(rng.tl.row, rng.br.row), col: Math.min(rng.tl.col, rng.br.col)},
        br = {row: Math.max(rng.tl.row, rng.br.row), col: Math.max(rng.tl.col, rng.br.col)};
    return {tl: tl, br: br};
  },

  excelToIndex(dollarRef) {
    let ref = dollarRef.replace(/\$/g, '').toUpperCase();
    var row=0, col=0, i=0, charIdx = 0;
    while(i < ref.length && isNaN(ref.charAt(i))){
      charIdx = i+1;
      i++;
    }
    var rawCol = ref.substring(0, charIdx), rawRow = ref.substring(charIdx);
    for (var c=0; c<charIdx; c++) {
      col = col + this.charToInt(ref.charAt(c)) * Math.pow(26, charIdx - c-1);
    }

    return {col: col, row: parseInt(rawRow)};
  },

  excelToRange(xp) {
    let endpoints = xp.split(":");
    if (endpoints.length === 1){
      let idx = this.excelToIndex(endpoints[0]);
      return {tl: idx, br: idx};
    }
    else {
      let start = this.excelToIndex(endpoints[0]),
          end = this.excelToIndex(endpoints[1]);
      return { tl: start, br: end };
    }
  },

  intToExcelCol(i) {
    i = i -1;
    var quo = Math.floor((i) / 26);
    var rem = (i) % 26;
    var code = '';
    if (quo > 0) {
        code += String.fromCharCode('A'.charCodeAt(0) + quo - 1);
    }
    code += String.fromCharCode('A'.charCodeAt(0) + rem);
    return code;
  },

  rangeToExcel(rng) {
    if (T.isIndex(rng)){
      return this.intToExcelCol(rng.tl.col) + rng.tl.row;
    } else {
      let {tl, br} = this.orientRange(rng);
      return this.intToExcelCol(tl.col) + tl.row
        + ":"
        + this.intToExcelCol(br.col) + br.row;
    }
  },

  removeLastWord(str){
    let lastIndex = str.lastIndexOf(" ");
    if (lastIndex > 0)
      return str.substring(0, lastIndex);
    else return "";
  },

  parseRefs(str) {
    if (str === "")
      return [];
    else{
      let regIdx = /\$?[A-Za-z]+\$?[0-9]+/g, regRng = /\$?[A-Za-z]+\$?[0-9]+:\$?[A-Za-z]+\$?[0-9]+/g;
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
      return matches;
    }
  },

  parseDependencies(str) {
    // logDebug("parsing dependencies of: " + str);
    let matches = this.parseRefs(str),
        parsed = matches.map((m) => this.orientRange(this.excelToRange(m)), this);
    logDebug("parsed deps: "+JSON.stringify(matches));
    return parsed;
  },

  _isContainedInLoc(col, row, loc) {
    let {tl, br} = loc;
    return (col >= tl.col && col <= br.col &&
            row >= tl.row && row <= br.row);
  },

  isContainedInLocs(col, row, locs) {
    for (var i=0; i<locs.length; i++) {
      if (this._isContainedInLoc(col, row, locs[i]))
        return true;
    }
    return false;
  },

  getAgnosticLanguageFromServer(lang) {
    return Constants.Languages[lang];
  },

  // indexStringToPair("(a,b)") := {row:a, col:b}
  indexStringToPair(indexString) {
    var ab = indexString.substr(1, indexString.length-2).split(",");
    return {fst : parseInt(ab[0], 10), snd: parseInt(ab[1], 10)};

  },

  /* Gets the top left cell from the listKey. */
  // listKeyToListHead("I/reafe/(a,b)?(c,d)?LIST") := (a,b)"
  listKeyToListHead(listKey) {
    if (listKey.split("?").length < 3 || listKey.split("?")[2] != "LIST") {
      logDebug("There was an error with the format of the listKey. Could not get list head.");
      return;
    }
    return this.indexStringToPair((listKey.split("?")[0]).split("/")[2]);
  },

  /* Gets the dimensions of the list from the listKey." */
  // listKeyToListDimensions("I/reafe/(a,b)?(c,d)?LIST") := (c,d)"
  listKeyToListDimensions(listKey) {
    if (listKey.split("?").length < 3 || listKey.split("?")[2] != "LIST") {
      logDebug("There was an error with the format of the listKey. Could not get listDimensions");
      return;
    }
    return this.indexStringToPair(listKey.split("?")[1]);
  },

  // TODO: make this actually correct?
  canInsertCellRefInXp(xp){
    let infix = ["+","-","*","/","(",",","&","="];
    if (infix.indexOf(xp.substring(xp.length-1,xp.length))>-1)
      return true;
    else
      return false;
  },

  getSafeRow(r) {
    return Math.min(Math.max(r, 1), Constants.numRows);
  },

  getSafeCol(c){
    return Math.min(Math.max(c, 1), Constants.numCols);
  },

  getSafeRange(rng) {
    return {tl: this.getSafeIndex(rng.tl),
            br: this.getSafeIndex(rng.br)};
  },

  getSafeSelection(sel) {
    return { origin: this.getSafeIndex(sel.origin), range: this.getSafeRange(sel.range) };
  },

  originIsCornerOfSelection(sel) {
    let {origin, range: {tl, br}} = sel;
    return (origin.row === tl.row || origin.row == br.row) &&
           (origin.col == tl.col || origin.col == br.col);
  },

  getSafeIndex(idx) {
    return {col: this.getSafeCol(idx.col), row: this.getSafeRow(idx.row)};
  },

  xor(foo, bar) {
    return foo ? !bar : bar;
  },

//Utils for getRowMajorCellValues

 sliceArray(begin, end) {
   return (
       function(arr) { return arr.slice(begin, end) }
   );
 },

 makeArrayOf(value, length) {
   var arr = [], i = length;
   while (i--) {
     arr[i] = value;
   }
   return arr;
 },

 make2DArrayOf(value, height, length) {
   var arr = [[]], i = height;
   while (i --) {
     arr[i] = this.makeArrayOf(value, length);
   }
   return arr;
 },

 concatAll(arrs) {
  return [].concat.apply([], arrs);
 },

 /*************************************************************************************************************************/
  // Cache extension to viewing window

  extendRangeByCache(rng) {
    let tl = this.getSafeIndex({row: rng.tl.row-Constants.scrollCacheY, col: rng.tl.col-Constants.scrollCacheX}),
        br = this.getSafeIndex({row: rng.br.row+Constants.scrollCacheY, col: rng.br.col+Constants.scrollCacheX});
    return { tl: tl, br: br };
  }

};
