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
    if (keyStr.length) // TODO fix array vs string checking
      keyStr.map((k) => {addShortcut(set, name, k, callback)});
    else {
      let s = {name: name};
      s = KeyUtils.parseIntoShortcut(s, keyStr);
      s.callback = callback;
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
        set[key].callback(KeyUtils.getWildcard(e, set[key]));
        return true;
      }
    }
    return false;
  },

  tryCommonShortcut(e) {
    return this.tryShortcut(e, this.CommonShortcuts);
  },

  tryNavShortcut(e) {
    return this.tryShortcut(e, this.NavShortcuts);
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
  },







// note: there will be some duplication between common and editor shortcuts,
// since some are possible when either the editor or grid is in focus.
  addEditorShortcuts(editor, props) {
    editor.commands.addCommand({
        name: 'eval',
        bindKey: {win: 'Ctrl+Enter',  mac: 'Command+Enter'},
        exec: function(_editor) {
          console.log("Eval requested for language: " + JSON.stringify(props.language));
          /*
            When user wants to eval an expression
            Sends back the expression and language in _editor from ace editor -> code editor -> eval pane via props
            The eval pane will get some spreadsheet state and send it to server for eval
          */
          props.onEvalRequest({exp: _editor.getValue(), lang: props.language});
        },
          readOnly: true
        });
    editor.commands.addCommand({
        name: 'esc',
        bindKey: {win: 'Esc',  mac: 'Esc'},
        exec: function(_editor) {
          _editor.setValue("");
          props.focusGrid();
        },
          readOnly: true
        });
    editor.commands.addCommand({
        name: 'toggle_reference',
        bindKey: {win: 'F4',  mac: 'F4'},
        exec: function(_editor) {
          // console.log('f4 pressed');
          var sesh = editor.getSession();
          var cursor = editor.getCursorPosition();
          var range = sesh.getWordRange(cursor.row, cursor.column);
          var sel = editor.selection;
          sel.setRange(range);
          // console.log(range);
          var replace = Util.toggleReferenceType(editor.getSelectedText());
          sesh.replace(range, replace);
        },
          readOnly: true
        });
  }

};
