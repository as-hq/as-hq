import {logDebug} from './Logger';

import Util from './Util';
import Constants from '../Constants';
import KeyUtils from './KeyUtils';

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

export default {

  add(set, name, keyStr, callback) {
    var self = this;
    if (keyStr.constructor === Array)
      keyStr.map((k) => self.add(set, name, k, callback));
    else {
      let s = {name: name};
      s = KeyUtils.parseIntoShortcut(s, keyStr);
      s.callback = callback;
      _S[set].push(s);
    }
  },

  shortcutMatches(s, e) {
    if (this.compareModifiers(s, e)) {
      if (s.optionKeys && Util.arrContains(s.optionKeys, e.which))
        return true;
      else return (s.keyCode && s.keyCode === e.which);
    } else return false;
  },

  tryShortcut(e, set) {
    let ss = _S[set]; // shortcut set to try
    for (var key in ss) {
      if (this.shortcutMatches(ss[key], e)){
        logDebug("shortcut matched!", JSON.stringify(ss[key]));
        ss[key].callback(KeyUtils.getWildcard(e, ss[key]));
        return true;
      }
    }
    return false;
  },

  gridShouldDeferKey(e){
    return (e.ctrlKey ||
            KeyUtils.isEvalKey(e) || // tab
            !KeyUtils.isNavKey(e)) &&
           !KeyUtils.isCopyPasteType(e);
  },

  editorShouldDeferKey(e) {
    return (!KeyUtils.producesTextChange(e) &&
            !KeyUtils.isNavKey(e) &&
            !KeyUtils.isCopyPasteType(e) &&
            !(e.ctrlKey && e.which === 65)) && // Ctrl+A
           !KeyUtils.isTextAreaNavKey(e);    // don't defer if editor should use the key
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
