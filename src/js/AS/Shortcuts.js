import Util from './Util';
import Constants from '../Constants';

var _to_ascii = {
        '188': '44',
        '109': '45',
        '190': '46',
        '191': '47',
        '192': '96',
        '220': '92',
        '222': '39',
        '221': '93',
        '219': '91',
        '173': '45',
        '187': '61', //IE Key codes
        '186': '59', //IE Key codes
        '189': '45'  //IE Key codes
    },

    shiftUps = {
        "96": "~",
        "49": "!",
        "50": "@",
        "51": "#",
        "52": "$",
        "53": "%",
        "54": "^",
        "55": "&",
        "56": "*",
        "57": "(",
        "48": ")",
        "45": "_",
        "61": "+",
        "91": "{",
        "93": "}",
        "92": "|",
        "59": ":",
        "39": "\"",
        "44": "<",
        "46": ">",
        "47": "?"
    },

    modifiers = [16, 17, 18, 19];

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
      optionKeys: [49,50,51,52,53,54,55,56,57,58,59],
      callback: function(e, parent) {
        console.log("switching language!");
        switch(e.which) {
          case 49:
            parent.setLanguage(Constants.Languages.Excel);
            break;
          case 50:
            parent.setLanguage(Constants.Languages.Python);
            break;
          case 51:
            parent.setLanguage(Constants.Languages.R);
            break;
          case 52:
            parent.setLanguage(Constants.Languages.OCaml);
            break;
          case 53:
            parent.setLanguage(Constants.Languages.SQL);
            break;
          case 54:
            parent.setLanguage(Constants.Languages.Java);
            break;
          case 55:
            parent.setLanguage(Constants.Languages.CPP);
            break;
        }
      }
    },
    {
      name: "toggle_focus",
      key: "F2",
      keyCode: 113,
      callback: function(e, parent) {
        console.log("focus toggled");
        parent.toggleFocus();
      }
    },
    {
      name: 'quick_python_graph',
      key: "F11",
      keyCode: 122,
      callback: function(e, parent) {
        var rng = Util.locToExcel(parent.refs.spreadsheet.getSelectionArea().range);
        parent.setLanguage(Constants.Languages.Python);
        parent.changeSelection('right');
        parent.handleEvalRequest('from AS.ui.plot import *; plotGeneric('+rng+')');
      }
    },
    {
      name: 'cell_eval',
      keyCode: 13,
      ctrlKey: true,
      callback: function(e, parent) {
        console.log("evaling cell!");
        let editorState = {
          exp: parent._getRawEditor().getValue(),
          lang: parent.state.language
        };
        parent.handleEvalRequest(editorState);
      }
    }

  ],

  getKey(e) {
    // TODO
    return e.key;
  },

  isFunctionKey(e) {
    return e.which >= 112 && e.which <= 123;
  },

  shortcutMatches(s, e) {
    if (this.compareModifiers(s, e)) {
      if (s.optionKeys && Util.arrContains(s.optionKeys, e.which))
        return true;
      else if (s.key && s.key === this.getKey(e))
        return true;
      else if (s.keyCode && s.keyCode === e.which)
        return true;
      else
        return false
    } else return false;
  },

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

  compareModifiers(s, e) {
    // s is the shortcut
    let sh = (s.hasOwnProperty('shiftKey') && s.shiftKey == e.shiftKey) || (e.shiftKey === false);
    let c = (s.hasOwnProperty('ctrlKey') && s.ctrlKey == e.ctrlKey) || (e.ctrlKey === false);
    let a = (s.hasOwnProperty('altKey') && s.altKey == e.altKey) || (e.altKey === false);
    let m = (s.hasOwnProperty('metaKey') && s.metaKey == e.metaKey) || (e.metaKey === false);
    return (sh && c && a && m);
  },

  duplicateKeyDown(e) {
    console.log("duplicating event with keycode: " + e.keyCode);
    console.log(e);
    let rawEvt = {
      altKey: e.altKey,
      ctrlKey: e.ctrlKey,
      metaKey: e.metaKey,
      shiftKey: e.shiftKey,
      code: e.keyCode,
      charCode: e.charCode,
      keyCode: e.keyCode
    };

    // chrome-specific
    if (e.keyIdentifier)
      rawEvt.keyIdentifier = e.keyIdentifier;

    let evt = new KeyboardEvent("keydown", rawEvt);

    // chrome webkit bug workaround
    // TODO case on chrome
    evt.keyCodeVal = e.keyCode;
    Object.defineProperty(evt, 'keyCode', {
                get : function() {
                    return this.keyCodeVal;
                }
    });
    Object.defineProperty(evt, 'which', {
                get : function() {
                    return this.keyCodeVal;
                }
    });
    return evt;
  },

  producesVisibleChar(e) {
    return (!(e.ctrlKey || e.altKey || e.metaKey) &&
            !Util.arrContains(modifiers, e.which) &&
            !this.isFunctionKey(e)) ||
           (e.ctrlKey && e.key === "Backspace"); // backspace
  },

  keyToString(e) {
    let c = e.which;
    console.log("key has code: " + c);
    //normalize keyCode
    if (_to_ascii.hasOwnProperty(c)) {
        c = _to_ascii[c];
    }

    if (!e.shiftKey && (c >= 65 && c <= 90)) {
        c = String.fromCharCode(c + 32);
    } else if (e.shiftKey && shiftUps.hasOwnProperty(c)) {
        //get shifted keyCode value
        c = shiftUps[c];
    } else {
        c = String.fromCharCode(c);
    }
    return c;
  },

  modifyStringForKey(str, e) {
    if (e.which === 8){ // backspace
      if (e.ctrlKey){
        let edited = Util.removeLastWord(str);
        return edited;
      }
      else return str.substring(0, str.length-1);
    } else {
      return str + this.keyToString(e);
    }
  },

// note: there will be some duplication between common and editor shortcuts,
// since some are possible when either the editor or grid is in focus.
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
