/* @flow */

// this file is not properly flowed. In fact, the types here are completely fucked

import type {
  Callback
} from '../../types/Base';

import type {
  ASShortcut,
  ASKeyCombination,
  ASKeyModifier,
  ASKeyProperty
} from '../../types/Keyboard';

import {logDebug} from '../Logger';

import Constants from '../../Constants';
import KeyUtils from './Key';
import ExpressionStore from '../../stores/ASExpressionStore';

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



// are all functions so that checks can be lazy evaluated
let contextChecks = {
  'notTyping': () => { return ! ExpressionStore.isEditing(); },
  'isTyping': () => { return ExpressionStore.isEditing(); }
};

const token_split = "+";
const option_split = "|";

export default {
  createConfiguration(
    target: string,
    name: string,
    keyStr: (string|Array<string>),
    callback: Callback<string>
  ): Array<any> { // TODO flow
    var self = this;
    if (keyStr instanceof Array) {
      const allShortcuts = keyStr.map(k => self.createConfiguration(target, name, k, callback));
      return [].concat.apply([], allShortcuts);
    } else {
      let s = this.parseShortcutConfig(target);
      s = this.parseKeysIntoShortcut(s, keyStr);
      s.name = name;
      s.callback = callback;
      return [s];
    }
  },

  tryShortcut(e: SyntheticKeyboardEvent, shortcutSet: Array<any>): boolean {
    return shortcutSet.some((keyComb) => {
      if (this.shortcutMatches(keyComb, e)) {
        if (this.checkContext(keyComb)) {
          // if a shortcut executed, prevent default behavior.
          KeyUtils.killEvent(e);
          keyComb.callback(this.getWildcard(e, keyComb));
          return true;
        } else {
          return false;
        }
      }

      return false;
    });
  },

  shortcutMatches(s: ASKeyCombination, e: SyntheticKeyboardEvent): boolean {
    if (this.compareModifiers(s, e)) {
      if (s.optionKeys && s.optionKeys.includes(e.which))
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

  compareModifiers(s: ASKeyCombination, e: SyntheticKeyboardEvent): boolean {
    // TODO: $FlowFixMe: This can't currently be flowed because of s[name] and e[name]
    const propertyMatches = (name: ASKeyProperty) => (!!s[name]) === (!!e[name]);

    return ['shiftKey', 'altKey'].every(propertyMatches)
      && (
    // #needsrefactor should we allow meta and ctrl keys to be essentially interchangeable?
    // if not, that should be reflected in the checks below. Currently, the following is a
    // catchall way of ensuring the command version of any control-based shortcut will work.
        ['ctrlKey', 'metaKey'].every(propertyMatches)
        || (s.ctrlKey && !s.metaKey && e.metaKey && !e.ctrlKey) // #ANAND this case allows meta keys to substitute for ctrl keys.
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
    let tokens = keyStr.split(token_split),
        options = tokens[tokens.length-1].split(option_split);
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
