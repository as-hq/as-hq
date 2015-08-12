import Util from './Util';
import Constants from '../Constants';

export default {
  NavKeys: ["ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown", "Home", "End"],

  // TODO integrate react-hotkeys
  KeyMap: {
    "eval_request": ['ctrl', 'enter']
  },

  NavShortcuts: [
    {
      name: "data_boundary",
      ctrlKey: true,
      // optionKeys: this.NavKeys,
      callback: function(e, grid) {
        // TODO change grid selection
        switch(this.getKey(e)) {
          case 'ArrowLeft':
            break;
          case 'ArrowRight':
            break;
          case 'ArrowUp':
            break;
          case 'ArrowDown':
            break;
          case 'Home':
            break;
          case 'End':
            break;
        }
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
        console.log("focus toggled");
        parent.toggleFocus();
      }
    },
    {
      name: 'quick_python_graph',
      key: "F11",
      callback: function(e, parent) {
        var rng = Util.locToExcel(parent.getActiveLocation());
        parent.setLanguage(Constants.Languages.Python);
        parent.changeSelection('right');
        parent.handleEvalRequest('from AS.ui.plot import *; plotGeneric('+rng+')');
      }
    },

  ],

  tryShortcut(e, entity, set) {
    for (var key in set) {
      if (this.shortcutMatches(set[key], e)){
        set[key].callback(e, entity);
        return true;
      }
    }
    return false;
  },

  tryCommonShortcut(e, parent) {
    return this.tryShortcut(e, parent, this.CommonShortcuts);
  },

  tryNavShortcut(e, grid) {
    return this.tryShortcut(e, grid, this.NavShortcuts);
  },

  gridShouldDeferKey(e){
    return !Util.arrContains(this.NavKeys, this.getKey(e));
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

  // duplicateKeyDown(e) {
  //   console.log("duplicating event with keycode: " + e.keyCode);
  //   console.log(e);
  //   let rawEvt = {
  //     altKey: e.altKey,
  //     ctrlKey: e.ctrlKey,
  //     metaKey: e.metaKey,
  //     shiftKey: e.shiftKey,
  //     charCode: e.charCode
  //   };

  //   // chrome-specific
  //   if (e.keyIdentifier)
  //     rawEvt.keyIdentifier = e.keyIdentifier;

  //   let evt = new KeyboardEvent("keydown", rawEvt);

  //   // chrome webkit bug workaround
  //   // TODO case on chrome
  //   evt.keyCodeVal = e.keyCode;
  //   Object.defineProperty(evt, 'keyCode', {
  //               get : function() {
  //                   return this.keyCodeVal;
  //               }
  //   });
  //   Object.defineProperty(evt, 'which', {
  //               get : function() {
  //                   return this.keyCodeVal;
  //               }
  //   });
  //   return evt;
  // },

  duplicateKeyDown(e) {
    return KeyEvents.crossBrowser_initKeyboardEvent("keydown", e);
  },

  addEditorShortcuts(editor, props) {
    editor.commands.addCommand({
        name: 'eval',
        bindKey: {win: 'Ctrl+Enter',  mac: 'Command+Enter'},
        exec: function(_editor) {
          console.log("eval requested for language: " + JSON.stringify(props.language));
          // sends back the expression in _editor to the parent (evalpane) when user types ctrl+enter
          props.requestEval({exp: _editor.getValue(), lang: props.language});
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
