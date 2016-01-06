/* @flow */

import type {
  NakedIndex,
} from '../../types/Eval';

import type {
  Dict
} from '../../types/Base';

import StringUtils from './String';

import Constants from '../../Constants';

import _ from 'lodash';

import ExpStore from '../../stores/ASExpStore.js';

// -----------------------------------------------------------------------------------------------------
// Key constants

var _to_ascii: Dict<number> = {
    //numpad stuff
    '96': 48, //0
    '97': 49,
    '98': 50,
    '99': 51,
    '100': 52,
    '101': 53,
    '102': 54,
    '103': 55,
    '104': 56,
    '105': 57, //9
    '106': 42, //*
    '107': 43, //+
    '109': 45, //-
    '110': 46, //.
    '111': 47, // /
    '188': 44,
    '190': 46,
    '191': 47,
    '192': 96,
    '220': 92,
    '222': 39,
    '221': 93,
    '219': 91,
    '173': 45,
    '187': 61, //IE Key codes
    '186': 59, //IE Key codes
    '189': 45  //IE Key codes
};

var shiftUps: Dict<string> = {
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

var modifiers: Array<number> = [16, 17, 18, 19]; //shift, ctrl, alt, pause, break

var specials: Array<number> = [27, 46, 36, 35, 33, 34, 9, 20]; //esc, delete, home, end, pgup, pgdown, tab, capslock

// based on https://css-tricks.com/snippets/javascript/javascript-keycodes/
var miscKeys: Array<number> = [9, 13, 32, //tab, enter, space
                96, 97, 98, 99, 100, 101, 102, 103, 104, 105, //numpad
                106, 107, 109, 110, 111, //add, subtract, decimal point, divide,
                186, 187, 188, 189, 190, 191, 192, //misc punctuation
                219, 220, 221, 222]; // moar punctuation

var keyMap: Dict<number> = {
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


let Key = {
  // -----------------------------------------------------------------------------------------------------
  // Key conversions and utils
  navKeys: [37, 38, 39, 40],

  isNavKey(e: SyntheticKeyboardEvent): boolean {
    return Key.navKeys.includes(e.which);
  },

  isPureArrowKey(e: SyntheticKeyboardEvent): boolean {
    return Key.isNavKey(e) && !Key.containsModifiers(e);
  },

  isEvalKey(e: SyntheticKeyboardEvent): boolean {
    return e.which === 13 || e.which === 9; // tab or enter
  },

  isFunctionKey(e: SyntheticKeyboardEvent): boolean {
    return e.which >= 112 && e.which <= 123;
  },

  containsModifiers(e: SyntheticKeyboardEvent): boolean {
    return e.ctrlKey || e.shiftKey || e.altKey || e.metaKey;
  },

  isDestructiveKey(e: SyntheticKeyboardEvent): boolean {
    return e.which === 8 || e.which === 46; // backspace or delete
  },

  isTextAreaNavKey(e: SyntheticKeyboardEvent): boolean {
    return e.which == 33 || e.which == 34 || // PgUp / PgDown
           e.which === 36 || e.which === 35; // home, end
  },

  isCtrlCmdKey(e: SyntheticKeyboardEvent): boolean {
    return e.ctrlKey || e.metaKey;
  },

  isCtrlA(e: SyntheticKeyboardEvent): boolean {
    return Key.isCtrlCmdKey(e) && e.which === 65;
  },

  isCtrlS(e: SyntheticKeyboardEvent): boolean {
    return Key.isCtrlCmdKey(e) && e.which === 83;
  },

  isAltH(e: SyntheticKeyboardEvent): boolean {
    return e.altKey && e.which === 72;
  },

  keyToString(e: SyntheticKeyboardEvent): string {
    let c = e.which;

    if (Key.isDestructiveKey(e) || specials.includes(c)) {
      return "";
    } else {
      // console.log("key has code: " + c);
      //normalize keyCode
      if (_to_ascii.hasOwnProperty(c)) {
          c = _to_ascii[c.toString()];
      }

      if (!e.shiftKey && (c >= 65 && c <= 90)) {
          c = String.fromCharCode(c + 32);
      } else if (e.shiftKey && shiftUps.hasOwnProperty(c)) {
          //get shifted keyCode value
          c = shiftUps[c.toString()];
      } else {
          c = String.fromCharCode(c);
      }
      return c;
    }
  },

  // determines whether editor should defer key in favor of shortcuts
  // NOTE: any Shift+??? shortcuts need to be manually cased here (shifts by default produce a visible character)
  producesTextChange(e: SyntheticKeyboardEvent): boolean {
    let isDestructive    = Key.isDestructiveKey(e);
    let isCopyPaste      = Key.isCopyPasteType(e);
    let makesVisibleChar = Key.producesVisibleChar(e);
    let isUndoOrRedo     = Key.isUndoType(e);

    return makesVisibleChar || isDestructive || isCopyPaste || isUndoOrRedo;
  },

  producesVisibleChar(e: SyntheticKeyboardEvent): boolean {
    // based off http://www.cambiaresearch.com/articles/15/javascript-char-codes-key-codes and
    // https://css-tricks.com/snippets/javascript/javascript-keycodes/
    // If you're holding down ctrl, alt, or a meta key, you're not going to be producing visible text.
    let noModifications = !(e.ctrlKey || e.altKey || e.metaKey);

    let notShiftSpace    = !(e.shiftKey && e.which === 32); // shift+space doesn't produce text
    let isAlphaNum    = (e.which >= 48 && e.which <= 90);
    let isMiscVisible = miscKeys.includes(e.which);     //more misc punctuation

    return noModifications && notShiftSpace && (isAlphaNum || isMiscVisible);
  },

  // is it a copy event or a paste event or a cut event?
  isCopyPasteType(e: SyntheticKeyboardEvent): boolean {
    return Key.isCtrlCmdKey(e) && (e.which === 67 || e.which === 86 || e.which === 88);
  },

  // is it an undo or redo? (includes ctrl+shift+z)
  isUndoType(e: SyntheticKeyboardEvent): boolean {
    return Key.isCtrlCmdKey(e) && (e.which === 89 || e.which === 90);
  },

  modifyStringByKey(str: string, cursorPos: ?number, e: SyntheticKeyboardEvent): [string, number] {
    if (cursorPos != null) {
      let newStart = Key._appendStringByKey(str.slice(0, cursorPos), e),
          newEnd = str.slice(cursorPos);

      return [newStart + newEnd, newStart.length];
    } else { // treat it as though cursorPos is at the end
      let newStr = Key._appendStringByKey(str, e);
      return [newStr, newStr.length];
    }
  },

  _appendStringByKey(str: string, e: SyntheticKeyboardEvent): string {
    if (Key.isDestructiveKey(e)) {
      if (e.which === 8) { // backspace
        if (e.ctrlKey) {
          let edited = StringUtils.removeLastWord(str);
          return edited;
        } else {
          return str.substring(0, str.length-1);
        }
      } else return str;
    } else {
      return str + Key.keyToString(e);
    }
  },

  //clickType type is wrong
  modifyTextboxForKey(e: SyntheticKeyboardEvent, userIsTyping: boolean,
                      clickType: ?string, oldXp: string,
                      editor: AERawClass): [string, number] {
    // Append to string if double click, replace if single click
    if (userIsTyping || clickType === Constants.ClickType.DOUBLE_CLICK) {
      // first check if editor has a thing selected
      if (editor.getSelectedText() != "") {
        // as of 11/2, only way to select xp from grid is with Ctrl+A
        // so if selection exists, the whole expression must be selected
        // so we destroy the current xp and replace it.
        if (Key.isDestructiveKey(e)) {
          return ["", 0];
        } else {
          return [Key.keyToString(e), 1];
        }
      } else {
        return Key.modifyStringByKey(oldXp, editor.getCursorPosition().column, e);
      }
    } else {
      if (!ExpStore.shouldHandlePercentFormat()) {
        return [Key.keyToString(e), 1];
      } else {
        return [Key.keyToString(e) + "%", 1];
      }
    }
  },

  killEvent(e: SyntheticEvent) {
    e.preventDefault();
    e.stopPropagation();
    if (e.nativeEvent) {
      e.nativeEvent.stopImmediatePropagation();
    }
  },

  stringToKey(k: string): number { // TODO test for pathological cases and mac keyboards
    if (keyMap.hasOwnProperty(k))
      return keyMap[k];
    else {
      let c = k.charCodeAt(0);
      if ((c-32) >= 65 && (c-32) <= 90) {
        return c-32;
      } else {
        return c;
      }
    }
  },

  keyToWildcard(e: SyntheticKeyboardEvent): string {
    let k = _.invert(keyMap);
    if (k[e.which])
      return k[e.which];
    else
      return Key.keyToString(e);
  },

  shiftIndexByKey(e: SyntheticKeyboardEvent, idx: NakedIndex): NakedIndex {
    switch (e.which) {
      case 37:
        return {row: idx.row, col: idx.col - 1};
      case 38:
        return {row: idx.row - 1, col: idx.col};
      case 39:
        return {row: idx.row, col: idx.col + 1};
      case 40:
        return {row: idx.row + 1, col: idx.col};
      default:
        throw "Invalid keyboard event passed in to shiftIndexByKey";
    }
  },

  // the only relevant attribute here should be the which
  mockedKeyboardEvent(which: number): SyntheticKeyboardEvent {
    let noop = () => {};
    return {
      altKey: false,
      charCode: 0,
      ctrlKey: false,
      getModifierState: null,
      key: "",
      keyCode: 0,
      locale: "",
      location: 0,
      metaKey: false,
      repeat: false,
      shiftKey: false,
      which: which,
      view: null,
      detail: 0,
      persist: noop,
      preventDefault: noop,
      stopPropagation: noop
    };
  }
};

export default Key;
