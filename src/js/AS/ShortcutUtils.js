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
//   callback: function(e, parent) {
//     logDebug("focus toggled");
//     parent.toggleFocus();
//   }
// },

let _S = {
  'grid': [],
  'editor': [],
  'common': [],
  'toplevel': [],
  'repl':[],
  'textbox':[]
};

// are all functions so that checks can be lazy evaluated
let contextChecks = {
  'notTyping': () => { return !ExpStore.getUserIsTyping(); },
  'isTyping': () => { return ExpStore.getUserIsTyping(); }
};

export default {

  add(config, name, keyStr, callback) {
    var self = this;
    if (keyStr.constructor === Array)
      keyStr.map((k) => self.add(config, name, k, callback));
    else {
      let s = KeyUtils.parseShortcutConfig(config);
      s = KeyUtils.parseKeysIntoShortcut(s, keyStr);
      s.name = name;
      s.callback = callback;
      _S[s.set].push(s);
    }
  },

  shortcutMatches(s, e) {
    if (this.compareModifiers(s, e)) {
      if (s.optionKeys && Util.arrContains(s.optionKeys, e.which))
        return true;
      else return (s.keyCode && s.keyCode === e.which);
    } else return false;
  },

  // check that we can execute the shortcut in the current context
  checkContext(s) {
    let checksMatch = true;
    Object.getOwnPropertyNames(contextChecks).forEach((check) => {
      if (s.config && s.config.hasOwnProperty(check)) {
        if (contextChecks[check]()) { 
          return; 
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

  tryShortcut(e, set) {
    let ss = _S[set]; // shortcut set to try
    for (var key in ss) {
      if (this.shortcutMatches(ss[key], e)) {
        if (this.checkContext(ss[key])) {
          logDebug("shortcut matched and will be exec: ", JSON.stringify(ss[key]));
          ss[key].callback(KeyUtils.getWildcard(e, ss[key]));
        } else {
          logDebug("shortcut matched but context prevented exec: ", JSON.stringify(ss[key]));
          continue;
        }
        return true;
      }
    }
    return false;
  },

  gridShouldDeferKey(e) {
    return (e.ctrlKey ||
            KeyUtils.isEvalKey(e) || // tab
            !KeyUtils.isNavKey(e)) &&
           !KeyUtils.isCopyPasteType(e);
  },

  gridShouldAddToTextbox(userIsTyping, e) { 
    let notEvalKey           = !KeyUtils.isEvalKey(e),
        typingAndMakesChange = userIsTyping && KeyUtils.producesTextChange(e), 
        startsTyping         = !userIsTyping && KeyUtils.producesVisibleChar(e);

    return notEvalKey && (typingAndMakesChange || startsTyping);
  },

  editorShouldDeferKey(e) {
    return !KeyUtils.producesTextChange(e) &&
           !KeyUtils.isNavKey(e) &&
           !KeyUtils.isCopyPasteType(e) &&
           !KeyUtils.isCtrlA(e) && 
           !KeyUtils.isUndoType(e) &&
           !KeyUtils.isTextAreaNavKey(e); 
  },

  textboxShouldDeferKey(e) {
    return KeyUtils.isEvalKey(e) ||          // defer on eval
          this.editorShouldDeferKey(e);  // defer when editor defers
  },

  replShouldDeferKey(e) {
    if (e.which === 13 && e.shiftKey === false) {
      return true;
    }
    return !KeyUtils.producesTextChange(e);
  },

  compareModifiers(s, e) {
    let propertyMatches =
      (name) => (!!s[name]) === (!!e[name]);
    return ['shiftKey', 'altKey'].every(propertyMatches)
      && (
        ['ctrlKey', 'metaKey'].every(propertyMatches)
        || (s.ctrlKey && !s.metaKey && e.metaKey && !e.ctrlKey)
      );
  }

};
