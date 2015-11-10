import Util from './Util';
import Constants from '../Constants';
import _ from 'underscore';

// -----------------------------------------------------------------------------------------------------
// Key constants

var _to_ascii = {
    //numpad stuff
    '96': '48', //0
    '97': '49',
    '98': '50',
    '99': '51',
    '100': '52',
    '101': '53',
    '102': '54',
    '103': '55',
    '104': '56',
    '105': '57', //9
    '106': '42', //*
    '107': '43', //+
    '109': '45', //-
    '110': '46', //.
    '111': '47', // /
    '188': '44',
    '190': '46',
    '191': '47',
    '192': '96',
    '220': '92',
    '222': '39',
    '221': '93',
    '219': '91',
    '173': '45',
    '187': '61', //IE Key codes
    '186': '59', //IE Key codes
    '189': '45'  //IE Key codes
};

var shiftUps = {
    "96": "~",
    "49": "!",
    "50": "@",
    "51": "#",
    "52": "$",
    "53": "%",
    "54": "^",
    "55": "&",
    "56": "*",
    "57": "(",
    "48": ")",
    "45": "_",
    "61": "+",
    "91": "{",
    "93": "}",
    "92": "|",
    "59": ":",
    "39": "\"",
    "44": "<",
    "46": ">",
    "47": "?"
};

var modifiers = [16, 17, 18, 19]; //shift, ctrl, alt, pause, break

var specials = [27, 46, 36, 35, 33, 34, 9, 20]; //esc, delete, home, end, pgup, pgdown, tab, capslock

// based on https://css-tricks.com/snippets/javascript/javascript-keycodes/
var miscKeys = [8, 9, 13, 32, //backspace, tab, enter, space
                96, 97, 98, 99, 100, 101, 102, 103, 104, 105, //numpad
                106, 107, 109, 110, 111, //add, subtract, decimal point, divide,
                186, 187, 188, 189, 190, 191, 192, //misc punctuation
                219, 220, 221, 222]; // moar punctuation

var keyMap = {
  "Enter": 13,
  "Down": 40,
  "Up": 38,
  "Left": 37,
  "Right": 39,
  "Home": 36,
  "End": 35,
  "PageUp": 33,
  "PageDown": 34,
  "Del": 46,
  "Space": 32,
  "Tab": 9,
  "Backspace": 8,
  "[": 219,
  "]": 221,
  "'": 222,
  "Esc": 27,
  "F1": 112,
  "F2": 113,
  "F3": 114,
  "F4": 115,
  "F5": 116,
  "F6": 117,
  "F7": 118,
  "F8": 119,
  "F9": 120,
  "F10": 121,
  "F11": 122
};


export default {
  // -----------------------------------------------------------------------------------------------------
  // Key conversions and utils
  navKeys: [37, 38, 39, 40],

  isNavKey(e) {
    return Util.arrContains(this.navKeys, e.which);
  },

  isPureArrowKey(e) {
    return this.isNavKey(e) && !this.containsModifiers(e);
  },

  isPureShiftKey(e) {
    return e.key === "Shift"; // may not work on non-chrome browsers
  },

  isEvalKey(e) {
    return e.which === 13 || e.which === 9; // tab or enter
  },

  isFunctionKey(e) {
    return e.which >= 112 && e.which <= 123;
  },

  containsModifiers(e) {
    return e.ctrlKey || e.shiftKey || e.altKey || e.metaKey;
  },

  isDestructiveKey(e) {
    return e.which === 8 || e.which === 46; // backspace or delete
  },

  isTextAreaNavKey(e) {
    return e.which === 36 || e.which === 35; // home, end
  },

  keyToString(e) {
    let c = e.which;

    if (this.isDestructiveKey(e) || Util.arrContains(specials, c)){
      return "";
    } else {
      // console.log("key has code: " + c);
      //normalize keyCode
      if (_to_ascii.hasOwnProperty(c)) {
          c = _to_ascii[c];
      }

      if (!e.shiftKey && (c >= 65 && c <= 90)) {
          c = String.fromCharCode(c + 32);
      } else if (e.shiftKey && shiftUps.hasOwnProperty(c)) {
          //get shifted keyCode value
          c = shiftUps[c];
      } else {
          c = String.fromCharCode(c);
      }
      return c;
    }
  },

  // determines whether editor should defer key in favor of shortcuts
  // NOTE: any Shift+??? shortcuts need to be manually cased here (shifts by default produce a visible character)
  producesTextChange(e) {
    // If you're holding down ctrl, alt, or a meta key, you're not going to be producing visible text.
    let noModifications = !(e.ctrlKey || e.altKey || e.metaKey);

    // If anything in autoFail is true, fail. If anything in autoSucceed is true, succeed. autoSucceed
    // takes precedence.
    let notShiftSpace   = !(e.shiftKey && e.which === 32); // shift+space doesn't produce text
    let isDestructive   = this.isDestructiveKey(e);

    // based off http://www.cambiaresearch.com/articles/15/javascript-char-codes-key-codes and
    // https://css-tricks.com/snippets/javascript/javascript-keycodes/
    let isAlphaNum    = (e.which >= 48 && e.which <= 90);
    let isMiscVisible = Util.arrContains(miscKeys, e.which);     //more misc punctuation

    return (noModifications &&
            notShiftSpace &&
            (isAlphaNum || isMiscVisible)) || isDestructive;
  },

  //is it a copy event or a paste event or a cut event?
  isCopyPasteType(e){
    return e.ctrlKey && (e.which === 67 || e.which === 86 || e.which === 88)
  },

  appendStringByKey(str, e) {
    if (this.isDestructiveKey(e)) {
      if (e.which === 8) { // backspace
        if (e.ctrlKey) {
          let edited = Util.removeLastWord(str);
          return edited;
        } else {
          return str.substring(0, str.length-1);
        }
      } else return str;
    } else {
      return str + this.keyToString(e);
    }
  },

  modifyTextboxForKey(e, userIsTyping, clickType, oldXp, editor) {
    // Append to string if double click, replace if single click
    if (userIsTyping || clickType === Constants.ClickType.DOUBLE_CLICK) {
      // first check if editor has a thing selected
      if (!editor.getSelection().$isEmpty) {
        // as of 11/2, only way to select xp from grid is with Ctrl+A
        // so if selection exists, the whole expression must be selected
        // so we destroy the current xp and replace it.
        if (this.isDestructiveKey(e)) return "";
        else return this.keyToString(e);

      } else {
        return this.appendStringByKey(oldXp, e);
      }
    } else return this.keyToString(e);
  },

  killEvent(e) {
    e.preventDefault();
    e.stopPropagation();
    if (e.nativeEvent){
      e.nativeEvent.stopImmediatePropagation();
    }
  },

  stringToKey(k) { // TODO test for pathological cases and mac keyboards
    if (keyMap.hasOwnProperty(k))
      return keyMap[k];
    else {
      let c = k.charCodeAt(0);
      if ((c-32) >= 65 && (c-32) <= 90)
        return c-32;
      else
        return c;
    }
  },

  keyToWildcard(e) {
    let k = _.invert(keyMap);
    if (k[e.which])
      return k[e.which]
    else return this.keyToString(e);
  },

  shiftIndexByKey(e, idx) {
    if (e.which === 37) return {row: idx.row, col: idx.col - 1};
    else if (e.which === 38) return {row: idx.row - 1, col: idx.col};
    else if (e.which === 39) return {row: idx.row, col: idx.col + 1};
    else if (e.which === 40) return {row: idx.row + 1, col: idx.col};
  },

// -----------------------------------------------------------------------------------------------------
// Shortcut utils

  parseModifierIntoShortcut(s, m) {
    switch(m) {
      case "Ctrl":
        s.ctrlKey = true;
        return s;
      case "Cmd":
        s.metaKey = true;
        return s;
      case "Shift":
        s.shiftKey = true;
        return s;
      case "Alt":
        s.altKey = true;
        return s;
      case "Meta":
        s.metaKey = true;
        return s;
    }
  },

  // assumes fornat: modifier + modifer + .. + key/key/key/key..
  parseKeysIntoShortcut(s, keyStr) {
    let tokens = keyStr.split("+"),
        options = tokens[tokens.length-1].split("/");
    if (options.length == 1)
      s.keyCode = this.stringToKey(options[0]);
    else
      s.optionKeys = options.map(this.stringToKey);
    for (var i=0; i<tokens.length-1; i++)
      s = this.parseModifierIntoShortcut(s, tokens[i]);
    return s;
  },

  // gets the matched wildcard, in string format
  getWildcard(e, s) {
    if (s.optionKeys){
      return this.keyToWildcard(e);
    } else return null;
  },

  // assumes spec format: set,option,option,...
  parseShortcutConfig(configStr) {
    let tokens = configStr.split(","),
        shortcut = {set: tokens[0], config: {}};
    for (let i=1; i<tokens.length; i++) { shortcut.config[tokens[i]] = true; }
    return shortcut;
  }
};
