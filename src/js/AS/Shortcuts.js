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
  GridShortcuts: [],
  EditorShortcuts: [],
  CommonShortcuts: []
};

export default {

  addShortcut(set, name, keyStr, callback) {
    var self = this;
    if (keyStr.constructor === Array) // TODO fix array vs string checking
      keyStr.map((k) => {self.addShortcut(set, name, k, callback)});
    else {
      let s = {name: name};
      s = KeyUtils.parseIntoShortcut(s, keyStr);
      s.callback = callback;
      console.log("add shortcut: " + name);
      console.log(s);
      switch(set){
        case 'grid':
          _S.GridShortcuts.push(s);
          break;
        case 'editor':
          _S.EditorShortcuts.push(s);
          break;
        case 'common':
          _S.CommonShortcuts.push(s);
          break;
      }
    }
  },

  shortcutMatches(s, e) {
    if (this.compareModifiers(s, e)) {
      if (s.optionKeys && Util.arrContains(s.optionKeys, e.which))
        return true;
      else if (s.keyCode && s.keyCode === e.which)
        return true;
      else
        return false
    } else return false;
  },

  tryShortcut(e, set) {
    for (var key in set) {
      if (this.shortcutMatches(set[key], e)){
        console.log("shortcut matched!");
        console.log(JSON.stringify(set[key]));
        set[key].callback(KeyUtils.getWildcard(e, set[key]));
        return true;
      }
    }
    return false;
  },

  tryCommonShortcut(e) {
    return this.tryShortcut(e, _S.CommonShortcuts);
  },

  tryGridShortcut(e) {
    return this.tryShortcut(e, _S.GridShortcuts);
  },

  gridShouldDeferKey(e){
    return !Util.arrContains(KeyUtils.navKeys, e.which);
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
