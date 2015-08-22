import Util from './Util';
import Constants from '../Constants';

// -----------------------------------------------------------------------------------------------------
// Key constants

var _to_ascii = {
    '188': '44',
    '109': '45',
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

var modifiers = [16, 17, 18, 19]; // TODO

var keyMap = {
  "Enter": 13,
  "Down": 40,
  "Up": 38,
  "Left": 37,
  "Right": 39,
  "Home": 36,
  "End": 35,
  "Esc": 0, // TODO
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

var navKeys = [37, 38, 39, 40];

export default {
  // -----------------------------------------------------------------------------------------------------
  // Key conversions and utils

  getKey(e) {
    // TODO
    return e.key;
  },

  isFunctionKey(e) {
    return e.which >= 112 && e.which <= 123;
  },

  keyToString(e) {
    let c = e.which;
    console.log("key has code: " + c);
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
  },

  producesVisibleChar(e) {
    return (!(e.ctrlKey || e.altKey || e.metaKey) &&
            !Util.arrContains(modifiers, e.which) &&
            !this.isFunctionKey(e)) ||
           (e.ctrlKey && e.key === "Backspace"); // backspace
  },

  modifyStringForKey(str, e) {
    if (e.which === 8){ // backspace
      if (e.ctrlKey){
        let edited = Util.removeLastWord(str);
        return edited;
      }
      else return str.substring(0, str.length-1);
    } else {
      return str + this.keyToString(e);
    }
  },

  killEvent(e) {
    e.preventDefault();
    e.stopPropagation();
    if (e.nativeEvent)
      e.nativeEvent.stopImmediatePropagation();
  },

  stringToKey(k) {
    if (keyMap.hasOwnProperty(k))
      return keyMap[k];
    else return k.charCodeAt(0)-32; // TODO test for pathological cases and mac keyboards
  },

  keyToWildcard(e) {
    for (var key in keyMap) {   // TODO avoid iteration
      if (keyMap[key] === e.which)
        return key;
    }
    return this.keyToString(e);
  },

// -----------------------------------------------------------------------------------------------------
// Shortcut utils

  parseModifierIntoShortcut(s, m) {
    switch(m) {
      case "Ctrl":
        s.ctrlKey = true;
        return s;
      case "Command":
        s.ctrlKey = true;
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

  parseIntoShortcut(s, str) {
    // NOTE
    // assumes fornat: modifier + modifer + .. + key/key/key/key..
    let tokens = str.split("+"),
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
  }
};
