import Util from './Util';
import Constants from '../Constants';
import KeyUtils from './KeyUtils';

// example raw shortcut

// {
//   name: "toggle_focus",
//   key: "F2",
//   keyCode: 113,
//   callback: function(e, parent) {
//     console.log("focus toggled");
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
        console.log("shortcut matched!", JSON.stringify(ss[key]));
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
    return !KeyUtils.producesTextChange(e) &&
           !KeyUtils.isNavKey(e) &&
           !KeyUtils.isCopyPasteType(e) &&
           !(e.ctrlKey && e.which === 65);  // ctrl + A
  },

  textboxShouldDeferKey(e) {
    return KeyUtils.isEvalKey(e) ||
           this.editorShouldDeferKey(e);
  },

  replShouldDeferKey(e) {
    if (e.which === 13 && e.shiftKey === false) {
      return true;
    }
    return !KeyUtils.producesTextChange(e);
  },

  compareModifiers(s, e) {
    let sh = (s.hasOwnProperty('shiftKey') && s.shiftKey == e.shiftKey) || (!s.hasOwnProperty('shiftKey') && (e.shiftKey === false));
    let c = (s.hasOwnProperty('ctrlKey') && s.ctrlKey == e.ctrlKey) || (!s.hasOwnProperty('ctrlKey') && (e.ctrlKey === false));
    let a = (s.hasOwnProperty('altKey') && s.altKey == e.altKey) || (!s.hasOwnProperty('altKey') && (e.altKey === false));
    let m = (s.hasOwnProperty('metaKey') && s.metaKey == e.metaKey) || (!s.hasOwnProperty('metaKey') && (e.metaKey === false));
    return (sh && c && a && m);
  }

};
