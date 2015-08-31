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
  'common': []
};

export default {

  addShortcut(set, name, keyStr, callback) {
    var self = this;
    if (keyStr.constructor === Array)
      keyStr.map((k) => {self.addShortcut(set, name, k, callback)});
    else {
      let s = {name: name};
      s = KeyUtils.parseIntoShortcut(s, keyStr);
      s.callback = callback;
      // console.log("add shortcut: " + name);
      // console.log(s);
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
        console.log("shortcut matched!");
        console.log(JSON.stringify(ss[key]));
        ss[key].callback(KeyUtils.getWildcard(e, ss[key]));
        return true;
      }
    }
    return false;
  },

  gridShouldDeferKey(e){
    return !Util.arrContains(KeyUtils.navKeys, e.which);
  },

  editorShouldDeferKey(e) {
    return !KeyUtils.producesVisibleChar(e);
  },

  changeGridSelection(e, grid) {
    if (e.ctrlKey) {
      this.killEvent(e);
      if (e.shiftKey) {
        // TODO select range to data boundary
      } else {
        // TODO move to data boundary
      }
    } else return false;
  },

  compareModifiers(s, e) {
    // s is the shortcut
    let sh = (s.hasOwnProperty('shiftKey') && s.shiftKey == e.shiftKey) || (e.shiftKey === false);
    let c = (s.hasOwnProperty('ctrlKey') && s.ctrlKey == e.ctrlKey) || (e.ctrlKey === false);
    let a = (s.hasOwnProperty('altKey') && s.altKey == e.altKey) || (e.altKey === false);
    let m = (s.hasOwnProperty('metaKey') && s.metaKey == e.metaKey) || (e.metaKey === false);
    return (sh && c && a && m);
  }

};
