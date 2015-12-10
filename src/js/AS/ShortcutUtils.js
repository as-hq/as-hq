/* @flow */

import type {
  Callback
} from '../types/Base';

import type {
  ASShortcutTarget,
  ASShortcut,
  ASKeyCombination,
  ASKeyModifier,
  ASKeyProperty
} from '../types/Keyboard';

import {logDebug} from './Logger';

import Util from './Util';
import Constants from '../Constants';
import KeyUtils from './KeyUtils';
import ExpStore from '../stores/ASExpStore';

// example raw shortcut

// {
//   name: "toggle_focus",
//   key: "F2",
//   keyCode: 113,
//   callback(e, parent) {
//     logDebug("focus toggled");
//     parent.toggleFocus();
//   }
// },

let _S: {[key: ASShortcutTarget]: Array<ASKeyCombination>} = {
  grid: [],
  editor: [],
  common: [],
  toplevel: [],
  repl: [],
  textbox: []
};

// are all functions so that checks can be lazy evaluated
let contextChecks = {
  'notTyping': () => { return !ExpStore.getUserIsTyping(); },
  'isTyping': () => { return ExpStore.getUserIsTyping(); }
};

export default {
  add(config: string, name: string, keyStr: (string|Array<string>), callback: Callback<string>) {
    var self = this;
    if (keyStr instanceof Array) {
      keyStr.forEach((k) => self.add(config, name, k, callback));
    } else {
      let s = this.parseShortcutConfig(config);
      s = this.parseKeysIntoShortcut(s, keyStr);
      s.name = name;
      s.callback = callback;
      _S[s.set].push(s);
    }
  },

  shortcutMatches(s: ASKeyCombination, e: SyntheticKeyboardEvent): boolean {
    if (this.compareModifiers(s, e)) {
      if (s.optionKeys && Util.arrContains(s.optionKeys, e.which))
        return true;
      else return ((!! s.keyCode) && s.keyCode === e.which);
    } else return false;
  },

  // check that we can execute the shortcut in the current context
  checkContext(s: ASShortcut): boolean {
    let checksMatch = true;
    Object.getOwnPropertyNames(contextChecks).forEach((check) => {
      if (s.config && s.config.hasOwnProperty(check)) {
        if (contextChecks[check]()) {
          return; // return applies only to the forEach, not the outer function
        } else {
          checksMatch = false;
          return;
        }
      } else {
        return;
      }
    });
    return checksMatch;
  },

  tryShortcut(e: SyntheticKeyboardEvent, set: ASShortcutTarget): boolean {
    let ss = _S[set]; // shortcut set to try

    return ss.some((keyComb) => {
      if (this.shortcutMatches(keyComb, e)) {
        if (this.checkContext(keyComb)) {
          logDebug("shortcut matched and will be exec: ", JSON.stringify(keyComb));
          keyComb.callback(this.getWildcard(e, keyComb));
          return true;
        } else {
          logDebug("shortcut matched but context prevented exec: ", JSON.stringify(keyComb));
        }
      }

      return false;
    });
  },

  gridShouldDeferKey(e: SyntheticKeyboardEvent): boolean {
    return (e.ctrlKey || e.metaKey ||
            KeyUtils.isEvalKey(e) || // tab or enter
            !KeyUtils.isNavKey(e)) &&
           !KeyUtils.isCopyPasteType(e);
  },

  gridShouldAddToTextbox(userIsTyping: boolean, e: SyntheticKeyboardEvent): boolean {
    let notEvalKey           = !KeyUtils.isEvalKey(e),
        typingAndMakesChange = userIsTyping && KeyUtils.producesTextChange(e),
        startsTyping         = !userIsTyping && KeyUtils.producesVisibleChar(e);

    return notEvalKey && (typingAndMakesChange || startsTyping);
  },

  editorShouldDeferKey(e: SyntheticKeyboardEvent): boolean {
    return !KeyUtils.producesTextChange(e) &&
           !KeyUtils.isNavKey(e) &&
           !KeyUtils.isCopyPasteType(e) &&
           !KeyUtils.isCtrlA(e) &&
           !KeyUtils.isUndoType(e) &&
           !KeyUtils.isTextAreaNavKey(e);
  },

  textboxShouldDeferKey(e: SyntheticKeyboardEvent): boolean {
    return KeyUtils.isEvalKey(e) ||          // defer on eval
          this.editorShouldDeferKey(e);  // defer when editor defers
  },

  replShouldDeferKey(e: SyntheticKeyboardEvent): boolean {
    if (e.which === 13 && e.shiftKey === false) {
      return true;
    }
    return !KeyUtils.producesTextChange(e);
  },

  compareModifiers(s: ASKeyCombination, e: SyntheticKeyboardEvent): boolean {
    // TODO: $FlowFixMe: This can't currently be flowed because of s[name] and e[name]
    let propertyMatches = (name: ASKeyProperty) => (!!s[name]) === (!!e[name]);
    return ['shiftKey', 'altKey'].every(propertyMatches)
      && (
        ['ctrlKey', 'metaKey'].every(propertyMatches)
        || (s.ctrlKey && !s.metaKey && e.metaKey && !e.ctrlKey)
      );
  },

  parseModifierIntoShortcut(s: ASKeyCombination, m: ASKeyModifier): ASKeyCombination {
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
      default:
        return s;
    }
  },

  // assumes fornat: modifier + modifer + .. + key/key/key/key..
  parseKeysIntoShortcut(s: ASKeyCombination, keyStr: string): ASKeyCombination {
    let tokens = keyStr.split("+"),
        options = tokens[tokens.length-1].split("/");
    if (options.length == 1)
      s.keyCode = KeyUtils.stringToKey(options[0]);
    else
      s.optionKeys = options.map(KeyUtils.stringToKey);
    for (var i=0; i<tokens.length-1; i++)
      s = this.parseModifierIntoShortcut(s, tokens[i]);
    return s;
  },

  // gets the matched wildcard, in string format
  getWildcard(e: SyntheticKeyboardEvent, s: ASKeyCombination): ?string {
    if (s.optionKeys) {
      return KeyUtils.keyToWildcard(e);
    } else {
      return null;
    }
  },

  // assumes spec format: set,option,option,...
  parseShortcutConfig(configStr: string): ASShortcut {
    let tokens = configStr.split(",");
    let [set, ...checks] = tokens;

    //xcxc: (set: any) because set is an arbitrary string right now
    let shortcut = {set: (set: any), config: {}};

    checks.forEach((check) => {
      shortcut.config[check] = true;
    });
    return shortcut;
  }
};
