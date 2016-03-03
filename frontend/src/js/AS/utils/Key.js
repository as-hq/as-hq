/* @flow */

import type {
  NakedIndex,
} from '../../types/Eval';

import type {
  Dict
} from '../../types/Base';

import type { Offset } from '../../types/Hypergrid';

import StringUtils from './String';

import Constants from '../../Constants';

import _ from 'lodash';

// -----------------------------------------------------------------------------------------------------
// Key constants

const _to_ascii: Dict<number> = {
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

const shiftUps: Dict<string> = {
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

const modifiers: Array<number> = [16, 17, 18, 19]; //shift, ctrl, alt, pause, break

const specials: Array<number> = [27, 46, 36, 35, 33, 34, 9, 20]; //esc, delete, home, end, pgup, pgdown, tab, capslock

// based on https://css-tricks.com/snippets/javascript/javascript-keycodes/
const miscVisibleKeys: Array<number> =
   [32, //space
    96, 97, 98, 99, 100, 101, 102, 103, 104, 105, //numpad
    106, 107, 109, 110, 111, //add, subtract, decimal point, divide,
    186, 187, 188, 189, 190, 191, 192, //misc punctuation
    219, 220, 221, 222]; // moar punctuation

const keyMap: Dict<number> = {
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
  "/": 191,
  "\\": 220,
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


const Key = {
  // -----------------------------------------------------------------------------------------------------
  // Key conversions and utils

  isNavKey(e: SyntheticKeyboardEvent): boolean {
    return (
      Key.isArrowKey(e) ||
      [33, 34, 35, 36].includes(e.which)    // home, end, pgup/down
    );
  },

  isArrowKey(e: SyntheticKeyboardEvent): boolean {
    return [37, 38, 39, 40].includes(e.which);
  },

  isModifierKey(e: SyntheticKeyboardEvent): boolean {
    return [16,17,18,224].includes(e.which); // Shift, Ctrl, Alt, Meta
  },

  // keys which shift the selection by a relative amount
  // e.g. Shift+Arrow key, but not Ctrl+Arrow key
  offsetsSelection(e: SyntheticKeyboardEvent): boolean {
    return (
      Key.isArrowKey(e) &&
      !e.ctrlKey &&
      !e.metaKey
    );
  },

  isFunctionKey(e: SyntheticKeyboardEvent): boolean {
    return (
      e.which >= 112 &&
      e.which <= 123
    );
  },

  containsModifiers(e: SyntheticKeyboardEvent): boolean {
    return e.ctrlKey || e.shiftKey || e.altKey || e.metaKey;
  },

  isDestructiveKey(e: SyntheticKeyboardEvent): boolean {
    return e.which === 8 || e.which === 46; // backspace or delete
  },

  isCtrlCmdKey(e: SyntheticKeyboardEvent): boolean {
    return e.ctrlKey || e.metaKey;
  },

  isCtrlS(e: SyntheticKeyboardEvent): boolean {
    return Key.isCtrlCmdKey(e) && e.which === 83;
  },

  keyToOffset(e: SyntheticKeyboardEvent): Offset {
    switch(e.which) {
      case 37:
        return {dX: -1, dY: 0};
      case 38:
        return {dX: 0, dY: -1};
      case 39:
        return {dX: 1, dY: 0};
      case 40:
        return {dX: 0, dY: 1};
      default:
        throw new Error('cannot convert non-arrow key to offset!');
    }
  },

  keyToString(e: SyntheticKeyboardEvent): string {
    let c = e.which;

    if (Key.isDestructiveKey(e) || specials.includes(c)) {
      return "";
    } else {
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

  // determines whether a key pressed in the grid fires START_EDITING.
  initiatesEditMode(e: SyntheticKeyboardEvent): boolean {
    const noModifications = !(e.ctrlKey || e.altKey || e.metaKey);
    const notShiftSpace   = !(e.shiftKey && e.which === 32);
    const isAlphaNum      = (e.which >= 48 && e.which <= 90);
    const isMiscVisible   = miscVisibleKeys.includes(e.which);     //more misc punctuation

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

  keyShiftValue(e: SyntheticKeyboardEvent): ({ dr: number, dc: number }) {
    switch (e.which) {
      case 37:
        return {dr: 0, dc: -1};
      case 38:
        return {dr: -1, dc: 0};
      case 39:
        return {dr: 0, dc: 1};
      case 40:
        return {dr: 1, dc: 0};
      default:
        throw "Invalid keyboard event passed in to shiftIndexByKey";
    }
  },

  // the only relevant attribute here should be the which
  mockedKeyboardEvent(which: number): SyntheticKeyboardEvent {
    const noop = () => {};
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
