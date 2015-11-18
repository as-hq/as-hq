/* @flow */

import type {
  PairObject
} from '../types/Base';

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ValueL,
  ValueError,
  ASValue,
  ASLanguage,
  ASSheet,
  ASCellTag,
  ASCell
} from '../types/Eval';

import type {
  ASCurrency
} from '../types/Format';

import type {
  CellBorder,
  ASOverlaySpec
} from '../types/Hypergrid';

import type {
  ASClientLanguage,
  ASViewingWindow,
  ASSelection
} from '../types/State';

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

  getHostUrl(): string {
    // let baseUrl = process.env.NODE_ENV ? HOST_IP : 'localhost';
    // logDebug("GOT ENV ARG: ", process.env.NODE_ENV);
    // return 'ws://' + baseUrl + ':' + HOST_WS_PORT;
    return Constants.isRemote ? ('ws://' + HOST_IP + ':' + HOST_WS_PORT) : Constants.HOST_WS_URL;
  },

/*************************************************************************************************************************/
// Cell rendering

  /* Used to know what to display on the sheet */
  showValue(cv: ASValue, isRepl: boolean = false): (string|number) {
    // logDebug("In show value: " + JSON.stringify(cv));
    let self = this;
    switch (cv.tag) {
      case "NoValue":
        return "";
      case "ValueNull":
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
          return ((cv: any): ValueError).errMsg;
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

  showFullValueList(cv: ValueL): string {
    return JSON.stringify(cv.contents.map(this.showValue));
  },

  tagsToRenderConfig(config: HGRendererConfig, tags: Array<ASCellTag>): HGRendererConfig {
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
        case "Format": // should be in showValue, not tagsToRenderConfig
          switch (tag.contents) {
            case "Money":
              config.value = self.formatMoney("$", config.value, 2);
              break;
            case "Percentage":
              config.value = self.formatPercentage(config.value);
              break;
            case "Date":
              config.value = self.formatDate(config.value);
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

  valueToRenderConfig(config: HGRendererConfig, val: ASValue): HGRendererConfig {
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

  getPaintedBordersForSingleCell(): CellBorder {
    return [[[0,0],[1,0]],
            [[1,0],[1,1]],
            [[1,1],[0,1]],
            [[0,1],[0,0]]];
  },

  getBordersForInteriorCell(col: number, row: number, rng: NakedRange): CellBorder {
    let {tl, br} = rng;
    if (T.isIndex(rng) && (col === tl.col && row === tl.row)) {
      return this.getPaintedBordersForSingleCell();
    } else {
      let borders: CellBorder = [null,null,null,null];
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
  getPaintedBorders(col: number, row: number, rngs: Array<NakedRange>): Array<CellBorder> {
    let result = rngs.map((rng) => this.getBordersForInteriorCell(col, row, rng), this);
    return this.concatAll(result);
  },

  getImageOverlay(c: ASCell, originX: number, originY: number): ?ASOverlaySpec {
    let {cellValue: cv} = c;
    if (cv.tag === 'ValueImage') {
      let self = this,
          ct = c.cellTags,
          imageWidth = 300,
          imageHeight =  300,
          imageOffsetX = 0,
          imageOffsetY = 0;
      for (var i = 0 ; i < ct.length; i++){
        if (ct[i].tag==="ImageData"){
          imageOffsetX = ct[i].imageOffsetX;
          imageOffsetY = ct[i].imageOffsetY;
          imageWidth = ct[i].imageWidth;
          imageHeight = ct[i].imageHeight;
        }
      }
      return {
        id: self.getUniqueId(),
        src: Constants.HOST_STATIC_URL + "/images/" + cv.imagePath,
        width: imageWidth,
        height: imageHeight,
        offsetX: imageOffsetX,
        offsetY: imageOffsetY,
        left: originX,
        top: originY,
        loc: c.cellLocation
      };
    }

    return null;
  },

  locEquals(c1: ASIndex, c2: ASIndex): boolean {
    let tagE = c1.tag === c2.tag,
        colE = c1.index.col === c2.index.col,
        rowE = c1.index.row === c2.index.row,
        sheetE = c1.sheetId === c2.sheetId
    return tagE && colE && rowE && sheetE;
  },

  simpleIndexEquals(c1: NakedIndex, c2: NakedIndex): boolean {
    return (c1.row === c2.row) && (c1.col === c2.col);
  },

  getX(col: number, scrollX: number): string {
    return (col-scrollX) * Constants.cellWidthPx + Constants.gridXOffset + "px";
  },

  getY(row: number, scrollY: number): string {
    return (row-scrollY)* Constants.cellHeightPx + Constants.gridYOffset + "px";
  },

/*************************************************************************************************************************/
// Formatting

  isFormattable(contents: string): boolean {
    return (!!contents) && !isNaN(contents) && contents != "";
  },

  formatMoney(currency: ASCurrency, contents: number): (?(string|number)) {
    if (!this.isFormattable(contents)) {
      return contents;
    }

    let delim: string = '',
        sign: string = '',
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
    }
    let formatted = (Math.round(contents * 100) / 100).toFixed(2),
        len = formatted.length;
    return sign + formatted.substring(0,len-3) + delim + formatted.substring(len-2);
  },

  formatPercentage(contents: number): (?(number|string)) {
    if (!this.isFormattable(contents)) {
      return contents;
    }
    return contents*100 + "%";
  },

  formatDate(contents: number): (?(number|string)) {
    if (!this.isFormattable(contents)) {
      return contents;
    }

    let diff = 25566, // number of days between 1/1/1900 (Excel's date base) and 1/1/1970 (Javascript's date base)
        millisecondsElapsed = (contents - diff)*24*60*60*1000,
        d = new Date(millisecondsElapsed);

    return String(Number(d.getMonth()) + 1) + "/" + d.getDate() + "/" + d.getFullYear();
  },

/*************************************************************************************************************************/
// Misc

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
  },

  isEmptyString(str: string): boolean {
    return /\S/.test(str);
  },

  shiftIndex(ind: NakedIndex, dr: number, dc: number): NakedIndex {
    return {row: ind.row + dr, col: ind.col + dc};
  },

  isEmptyCell(c: ASCell): boolean {
    return !c || ((c.cellExpression.expression == "") && (c.cellTags.length == 0));
  },

  removeEmptyLines(str: string): string {
    var lines = str.split("\n");
    var filtered = lines.filter(this.isEmptyString);
    return filtered.join("\n");
  },

  getIndicesOf(searchStr: string, str: string): Array<number> {
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

  colorToHtml(str: string): string {
    if (str.charAt(0) === "#" || str.substring(0,2) === "rgb") // if color already correct format
      return str;
    else return this.colorNameToHex(str);
  },

  colorNameToHex(color: string): ?string {
    if (typeof colors[color.toLowerCase()] != 'undefined')
        return colors[color.toLowerCase()];
    return undefined;
  },

  arrContains<T>(arr: Array<T>, elem: T): boolean {
    return arr.indexOf(elem) > -1;
  },

  // counts the char : as part of a word. 
  getExtendedWordRange(session, r: number, c: number) { 
    let immWordRange = session.getWordRange(r, c), 
        wordStart = immWordRange.start.column, 
        wordEnd   = immWordRange.end.column; 

    let beforeRange = immWordRange.clone();
        beforeRange.start.column = Math.max(0, wordStart-1);

    let afterRange = immWordRange.clone();
        afterRange.end.column = wordEnd + 1; // doesn't matter if wordEnd is too large

    if (wordStart > 0 && session.getTextRange(beforeRange)[0] == ':') { 
      wordStart = session.getWordRange(r, wordStart - 1).start.column; 
    } else { 
      let after = session.getTextRange(afterRange); 
      if (after[after.length - 1] == ':') { 
        wordEnd = session.getWordRange(r, wordEnd + 1).end.column; 
      }
    }

    let extRange = immWordRange.clone(); 
    extRange.start.column = wordStart; 
    extRange.end.column = wordEnd; 
    return extRange; 
  },

  toggleReference(xp: string): ?string {
    // TODO generalize to arbitrary range lengths
    let deps = this.parseRefs(xp);
    if (deps.length === 0) {
      return null;
    } else if (deps.length > 1) {
      throw "Single word contains multiple references.";
    }

    return this._toggleReference(deps[0]);
  },

  _toggleReference(ref: string): ?string {
    let refs = ref.split(':');
    if (refs.length > 1) { 
      return refs.map((r) => this._toggleReference(r)).join(':');
    }

    let dollarIndices = this.getIndicesOf('$', ref),
        cleanRef = ref.replace(/\$/g, ''),
        row = cleanRef.split(/[A-Za-z]+/).pop(),
        col = cleanRef.substring(0, cleanRef.length - row.length);
    if (dollarIndices.length === 0) {
      return '$' + col + (row ? '$' + row : ''); // A -> $A, not $A$
    } else if (dollarIndices.length === 1 ) {
      if (dollarIndices[0] === 0) {
        return col + row;
      }
      else {
        return '$' + col + row;
      }
    } else if (dollarIndices.length === 2) {
      return col + '$' + row;
    } else { 
      return null;
    }
  },

  intToChar(i: number): string {
    return 'ABCDEFGHIJKLMNOPQRSTUVXYZ'.charAt(i);
  },

  charToInt(c: string): number {
    return c.charCodeAt(0) - 64;
  },

  orientRange(rng: NakedRange): NakedRange {
    var tl = {row: Math.min(rng.tl.row, rng.br.row), col: Math.min(rng.tl.col, rng.br.col)},
        br = {row: Math.max(rng.tl.row, rng.br.row), col: Math.max(rng.tl.col, rng.br.col)};
    return {tl: tl, br: br};
  },

  excelToIndex(dollarRef: string): NakedIndex {
    let ref = dollarRef.replace(/\$/g, '').toUpperCase();
    var row=0, col=0, i=0, charIdx = 0;
    while (i < ref.length && isNaN(ref.charAt(i))) {
      charIdx = i+1;
      i++;
    }
    var rawCol = ref.substring(0, charIdx), rawRow = ref.substring(charIdx);
    for (var c=0; c<charIdx; c++) {
      col = col + this.charToInt(ref.charAt(c)) * Math.pow(26, charIdx - c-1);
    }

    if (rawRow.length > 0) { 
      return {col: col, row: parseInt(rawRow)};
    } else { 
      return {col: col, row: Infinity};
    }
  },

  excelToRange(xp: string): NakedRange {
    let endpoints = xp.split(":");
    if (endpoints.length === 1){
      let idx = this.excelToIndex(endpoints[0]);
      return {tl: idx, br: idx};
    } else {
      let start = this.excelToIndex(endpoints[0]),
          end = this.excelToIndex(endpoints[1]);
      return { tl: start, br: end };
    }
  },

  intToExcelCol(i: number): string {
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

  rangeToExcel(rng: NakedRange): string {
    if (T.isIndex(rng)){
      return this.intToExcelCol(rng.tl.col) + rng.tl.row;
    } else {
      let {tl, br} = this.orientRange(rng);
      return this.intToExcelCol(tl.col) + tl.row
        + ":"
        + this.intToExcelCol(br.col) + br.row;
    }
  },

  removeLastWord(str: string): string {
    let lastIndex = str.lastIndexOf(" ");
    if (lastIndex > 0)
      return str.substring(0, lastIndex);
    else return "";
  },

  parseRefs(str: string): Array<string> {
    if (str === "") {
      return [];
    } else {
      let regIdx      = /!?\$?[A-Za-z]+\$?[0-9]+/g, 
          regRng      = /!?\$?[A-Za-z]+\$?[0-9]+:\$?[A-Za-z]+\$?[0-9]+/g, 
          regCols     = /!?\$?[A-Za-z]+(\$?[0-9]+)?:\$?[A-Za-z]+(\$?[0-9]+)?/g, //matches ranges too, but only checks after we remove all ranges (see str2)
          rngs        = str.match(regRng), 
          str2        = str.replace(regRng, ""), 
          cols        = str2.match(regCols),
          str3        = str2.replace(regCols, ""), 
          idxs        = str3.match(regIdx), 
          firstNotExc = ((s) => s[0] != '!'),
          rngsOnSheet = rngs ? rngs.filter(firstNotExc) : [], 
          idxsOnSheet = idxs ? idxs.filter(firstNotExc) : [], 
          colsOnSheet = cols ? cols.filter(firstNotExc) : [],
          matches = rngsOnSheet.concat(idxsOnSheet).concat(colsOnSheet);
      return matches;
    }
  },

  parseDependencies(str: string, lang: ?ASLanguage): Array<NakedRange> {
    // logDebug("parsing dependencies of: " + str);
    if (lang == 'Excel' && str.length > 0 && str[0] != '=') { 
      return []; 
    }
    let matches = this.parseRefs(str),
        parsed = matches.map((m) => this.orientRange(this.excelToRange(m)), this);
    logDebug("parsed deps: "+JSON.stringify(matches));
    return parsed;
  },

  getAgnosticLanguageFromServer(lang: ASLanguage): ASClientLanguage {
    return Constants.Languages[lang];
  },

  // indexStringToPair("(a,b)") := {row:a, col:b}
  indexStringToPair(indexString: string): PairObject<number> {
    var ab = indexString.substr(1, indexString.length-2).split(",");
    return {fst : parseInt(ab[0], 10), snd: parseInt(ab[1], 10)};
  },

  /* Gets the top left cell from the listKey. */
  // listKeyToListHead("I/reafe/(a,b)?(c,d)?LIST") := (a,b)"
  listKeyToListHead(listKey: string): (PairObject<number>) {
    if (listKey.split("?").length < 3 || listKey.split("?")[2] != "LIST") {
      throw new Error('There was an error with the format of listkey, no list head.');
    }
    return this.indexStringToPair((listKey.split("?")[0]).split("/")[2]);
  },

  /* Gets the dimensions of the list from the listKey." */
  // listKeyToListDimensions("I/reafe/(a,b)?(c,d)?LIST") := (c,d)"
  listKeyToListDimensions(listKey: string): (PairObject<number>) {
    if (listKey.split("?").length < 3 || listKey.split("?")[2] != "LIST") {
      throw new Error('There was an error with the format of listkey, no dimensions.');
    }
    return this.indexStringToPair(listKey.split("?")[1]);
  },

  // TODO: make this actually correct?
  canInsertCellRefInXp(xp: string): boolean {
    let infix = ["+","-","*","/","(",",","&","="];
    if (infix.indexOf(xp.substring(xp.length-1,xp.length))>-1)
      return true;
    else
      return false;
  },

  xor(foo: boolean, bar: boolean): boolean {
    return foo ? !bar : bar;
  },

//Utils for getRowMajorCellValues

 sliceArray<T>(begin: number, end: number): ((arr: Array<T>) => Array<T>) {
   return (
       function(arr) { return arr.slice(begin, end) }
   );
 },

 makeArrayOf<T>(value: T, length: number): Array<T> {
   var arr = [], i = length;
   while (i--) {
     arr[i] = value;
   }
   return arr;
 },

 make2DArrayOf<T>(value: T, height: number, length: number): Array<Array<T>> {
   var arr = [[]], i = height;
   while (i --) {
     arr[i] = this.makeArrayOf(value, length);
   }
   return arr;
 },

 concatAll<T>(arrs: Array<Array<T>>): Array<T> {
  return [].concat.apply([], arrs);
 },

 /*************************************************************************************************************************/
  // Locations

  getSafeRow(r: number): number {
    return Math.min(Math.max(r, 1), Constants.numRows);
  },

  getSafeCol(c: number): number {
    return Math.min(Math.max(c, 1), Constants.numCols);
  },

  getSafeRange(rng: NakedRange): NakedRange {
    return {tl: this.getSafeIndex(rng.tl),
            br: this.getSafeIndex(rng.br)};
  },

  _isContainedInLoc(col: number, row: number, loc: NakedRange): boolean {
    let {tl, br} = loc;
    return (col >= tl.col && col <= br.col &&
            row >= tl.row && row <= br.row);
  },

  isContainedInLocs(col: number, row: number, locs: Array<NakedRange>): boolean {
    return locs.some((loc) => this._isContainedInLoc(col, row, loc));
  },

  getSafeSelection(sel: ASSelection): ASSelection {
    return { origin: this.getSafeIndex(sel.origin), range: this.getSafeRange(sel.range) };
  },

  originIsCornerOfSelection(sel: ASSelection): boolean {
    let {origin, range: {tl, br}} = sel;
    return (origin.row === tl.row || origin.row == br.row) &&
           (origin.col == tl.col || origin.col == br.col);
  },

  getSafeIndex(idx: NakedIndex): NakedIndex {
    return {col: this.getSafeCol(idx.col), row: this.getSafeRow(idx.row)};
  },

  extendRangeByCache(rng: NakedRange): NakedRange {
    let tl = this.getSafeIndex({row: rng.tl.row-Constants.scrollCacheY, col: rng.tl.col-Constants.scrollCacheX}),
        br = this.getSafeIndex({row: rng.br.row+Constants.scrollCacheY, col: rng.br.col+Constants.scrollCacheX});
    return { tl: tl, br: br };
  },

  offsetRange(rng: NakedRange, dY: number, dX: number): NakedRange {
    let {tl, br} = rng;
    return {tl: {col: tl.col + dX, row: tl.row + dY},
            br: {col: br.col + dX, row: br.row + dY}};
  },

    // Check if the mouse location is in the square box for draggging
  mouseLocIsContainedInBox(
    mouseLocX: number,
    mouseLocY: number,
    topLeftBoxObj: { x: number, y: number },
    boxWidth: number
  ): boolean {
    let xInBounds = mouseLocX >= topLeftBoxObj.x &&
                    mouseLocX <= topLeftBoxObj.x + boxWidth,
        yInBounds = mouseLocY >= topLeftBoxObj.y &&
                    mouseLocY <= topLeftBoxObj.y + boxWidth;
    return xInBounds && yInBounds;
  }

};
