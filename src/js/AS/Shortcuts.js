import Util from './Util';
import Constants from '../Constants';

export default {
  NavShortcuts: [
    {
      name: "data_boundary",
      ctrlKey: true,
      optionKeys: this.NavKeys,
      callback: function(e, grid) {
        // TODO change grid selection
      }
    }
  ],

  CommonShortcuts: [
    {
      name: "set_language",
      ctrlKey: true,
      optionKeys: ['1','2','3','4','5','6','7'],
      callback: function(e, parent) {
        switch(this.getKey(e)) {
          case '1':
            parent.setLanguage(Constants.Languages.Excel);
            break;
          case '2':
            parent.setLanguage(Constants.Languages.Python);
            break;
          case '3':
            parent.setLanguage(Constants.Languages.R);
            break;
          case '4':
            parent.setLanguage(Constants.Languages.OCaml);
            break;
          case '5':
            parent.setLanguage(Constants.Languages.SQL);
            break;
          case '6':
            parent.setLanguage(Constants.Languages.Java);
            break;
          case '7':
            parent.setLanguage(Constants.Languages.CPP);
            break;
        }
      }
    },
    {
      name: "toggle_focus",
      key: "F2",
      callback: function(e, parent) {
        parent.toggleFocus();
      }
    }
  ],

  NavKeys: ["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown", "Home", "End"],

  tryShortcut(e, entity, set) {
    for (var key in set) {
      if (this.shortcutMatches(set[key], e)){
        set[key].callback(e, parent);
        return true;
      }
    }
    return false;
  },

  tryCommonShortcut(e, parent) {
    this.tryShortcut(e, parent, this.CommonShortcuts);
  },

  tryNavShortcut(e, grid) {
    this.tryShortcut(e, grid, this.NavShortcuts);
  },

  gridShouldDeferKey(e){
    return Util.arrContains(this.NavKeys, this.getKey(e));
  },

  changeGridSelection(e, grid) {
    if (e.ctrlKey) {
      this.killEvent(e);
      if (e.shiftKey) {
        // TODO select range to data boundary
      } else {
        // TODO move to data boundary
      }
    }
  },

  killEvent(e) {
    e.preventDefault();
    e.stopPropagation();
    if (e.nativeEvent)
      e.nativeEvent.stopImmediatePropagation();
  },

  compareModifiers(e, ee) {
    let s = e.shiftKey == ee.shiftKey;
    let c = e.ctrlKey == ee.ctrlKey;
    let a = e.altKey == ee.altKey;
    let m = e.metaKey == ee.metaKey;
    return (s && c && a && m);
  },

  shortcutMatches(s, e) {
    if (this.compareModifiers(s, e)) {
      if (s.optionKeys && Util.arrContains(s.optionKeys, this.getKey(e)))
        return true;
      else if (s.key && s.key === this.getKey(e))
        return true;
      else
        return false
    } else return false;
  },

  getKey(e) {
    // TODO
    return e.key;
  },

  duplicateKeyDown(e) {
    let evt = new KeyboardEvent("keydown", {
      altKey: e.altKey,
      bubbles: false,
      charCode: e.charCode,
      ctrlKey: e.ctrlKey,
      keyCode: e.keyCode,
      metaKey: e.metaKey,
      shiftKey: e.shiftKey
    });
    return evt;
  },

  addEditorShortcuts(editor, props) {
    editor.commands.addCommand({
        name: 'eval',
        bindKey: {win: 'Ctrl+Enter',  mac: 'Command+Enter'},
        exec: function(_editor) {
          console.log("eval requested");
          // sends back the expression in _editor to the parent (evalpane) when user types ctrl+enter
          props.requestEval({exp: _editor.getValue(), lang: props.mode});
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
    _editor.commands.addCommand({
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
          var replace = toggleReferenceType(editor.getSelectedText());
          sesh.replace(range, replace);
        },
          readOnly: true
        });
  }

};
