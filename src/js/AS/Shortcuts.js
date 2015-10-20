import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import ShortcutUtils from './ShortcutUtils';
import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';
import Converter from '../AS/Converter';

export default {
  addShortcuts(evalPane) {
    let self = evalPane;
    // console.log("adding shortcuts!");

    // common shortcuts -------------------------------------------------------------------------------

    ShortcutUtils.addShortcut("common", "toggle_focus", "F2", (wildcard) => {
      console.log("F2 PRESSED " + self.state.userIsTyping);
      self.setState({userIsTyping:!self.state.userIsTyping},function(){
        self.updateTextBox(self.state.userIsTyping);
      });
    });
    ShortcutUtils.addShortcut("common", "new_sheet", "Shift+F11", (wildcard) => {
      // TODO
    });
    ShortcutUtils.addShortcut("common", "cell_eval", ["Ctrl+Enter", "Ctrl+D", "Ctrl+R"], (wildcard) => {
      let editorState = {
        exp: self._getRawEditor().getValue(),
        lang: self.state.language
      };
      self.handleEvalRequest(editorState, 1, 0);
    });
    ShortcutUtils.addShortcut("common", "cell_eval_arrayformula", "Ctrl+Shift+Enter", (wildcard) => {
      var editorValue = self._getRawEditor().getValue();
      console.log(self.state.language);
      if (self.state.language == Constants.Languages.Excel){
        editorValue = "{" + self._getRawEditor().getValue() + "}";
        self._getRawEditor().setValue(editorValue);
      }
      let editorState = {
        exp: editorValue,
        lang: self.state.language
      };
      self.handleEvalRequest(editorState, 1, 0);
    });

    ShortcutUtils.addShortcut("common", "set_language", "Ctrl+1/2/3/4/5/6/7/8/9", (wildcard) => {
      // TODO propagate dropdown
      switch(wildcard) {
          case "1":
            self.setLanguage(Constants.Languages.Excel);
            break;
          case "2":
            self.setLanguage(Constants.Languages.Python);
            break;
          case "3":
            self.setLanguage(Constants.Languages.R);
            break;
          case "4":
            self.setLanguage(Constants.Languages.OCaml);
            break;
          case "5":
            self.setLanguage(Constants.Languages.SQL);
            break;
          case "6":
            self.setLanguage(Constants.Languages.Java);
            break;
          case "7":
            self.setLanguage(Constants.Languages.CPP);
            break;
        }
    });
    ShortcutUtils.addShortcut("common", "format_value", "Ctrl+Shift+2/3/4/5/6", (wildcard) => {
      let tag;
      // TODO other wildcards
      if (wildcard === '4')
        tag = {tag: "Money", contents: []};
      else if (wildcard === '5')
        tag = {tag: "Percentage", contents: []};
      let rng = Store.getActiveSelection();
      Store.addTag(tag, rng.col, rng.row);
      self.refs.spreadsheet.repaint();
    });
    ShortcutUtils.addShortcut("common", "toggle_repl", "Alt+F11", (wildcard) => {
      self._toggleRepl();
    });
    ShortcutUtils.addShortcut("common", "esc", "Esc", (wildcard) => {
      console.log("Esc pressed");
      self.updateTextBox(false);
      Store.setClipboard(null, false);
      self.setState({focus: "grid",userIsTyping:false});
      self.refs.spreadsheet.repaint(); // render immediately
    });

    // repl shortcuts -------------------------------------------------------------------------------
    ShortcutUtils.addShortcut("repl", "repl_submit", "Ctrl+Enter", (wildcard) => {
      /* Preprocessing of repl value to get the "last" part to send to server */
      let strs = self._replValue().split(">>>").slice(-1)[0].substring(1);
      let lines = strs.split("\n");
      let send = lines.map((l) => {
        if (l.substring(0,4) === "    ")
          return l.substring(4);
        else return l;
      }).join("\n");

      console.log("SEND REPL: " + JSON.stringify(send));
      let editorState = {
        exp: send,
        lang: self.state.replLanguage.Server
      };
      // parse exp to get the last thing
      self.handleReplRequest(editorState);
    });


    // editor shortcuts -------------------------------------------------------------------------------
    ShortcutUtils.addShortcut("editor", "toggle_reference", "F4", (wildcard) => {
      let editor = self._getRawEditor(),
          sesh = editor.getSession(),
          cursor = editor.getCursorPosition(),
          range = sesh.getWordRange(cursor.row, cursor.column),
          sel = editor.selection;
      sel.setRange(range);
      let replace = Util.toggleReferenceType(editor.getSelectedText());
      sesh.replace(range, replace);
    });


    // grid shortcuts -------------------------------------------------------------------------------
    ShortcutUtils.addShortcut("grid", "moveto_data_boundary", "Ctrl+Up/Down/Left/Right", (wildcard) => {
      let newLoc = Store.getExtendedRange(wildcard, false);
      console.log("moving to: ", newLoc);
      self.refs.spreadsheet.makeSelection(newLoc, newLoc);
    });
    ShortcutUtils.addShortcut("grid", "moveto_data_boundary_selected", "Ctrl+Shift+Up/Down/Left/Right", (wildcard) => {
      let oldOrigin = Store.getActiveSelection().origin;
      let newLoc = Store.getExtendedRange(wildcard, true);
      console.log("\n\nMOVING TO\n\n: ", newLoc);
      console.log("\n\ORIGIN\n\n: ", oldOrigin);
      self.refs.spreadsheet.makeSelection(newLoc, oldOrigin);
    });
    ShortcutUtils.addShortcut("grid", "grid_fill_down", "Ctrl+D", (wildcard) => {
      let sel = Store.getActiveSelection().range;
      if (sel.row2){
        let copyFrom = {row: sel.row, col: sel.col, row2: sel.row, col2: sel.col},
            copyTo = {row: sel.row+1, col: sel.col, row2: sel.row2, col2: sel.col};
        API.sendCopyRequest([copyFrom, copyTo]);
      }
    });
    ShortcutUtils.addShortcut("grid", "grid_fill_right", "Ctrl+R", (wildcard) => {
      let sel = Store.getActiveSelection().range;
      if (sel.col2){
        let copyFrom = {row: sel.row, col: sel.col, row2: sel.row, col2: sel.col},
            copyTo = {row: sel.row, col: sel.col+1, row2: sel.row, col2: sel.col2};
        API.sendCopyRequest([copyFrom, copyTo]);
      }
    });
    ShortcutUtils.addShortcut("grid", "grid_select_all", "Ctrl+A", (wildcard) => {
      self.refs.spreadsheet.makeSelection(self.refs.spreadsheet.getViewingWindowWithCache().range);
    });
    ShortcutUtils.addShortcut("grid", "grid_home", ["Home", "Ctrl+Home"], (wildcard) => {
      self.refs.spreadsheet.makeSelection({row: 1, col: 1});
    });
    ShortcutUtils.addShortcut("grid", "grid_moveto_end_sheet", "Ctrl+End", (wildcard) => {
      //TODO
    });
    ShortcutUtils.addShortcut("grid", "move_vwindow_above", "PageUp", (wildcard) => {
      let dY = self.refs.spreadsheet.getVisibleRows();
      self.refs.spreadsheet.shiftSelectionArea(-dY, 0);
    });
    ShortcutUtils.addShortcut("grid", "move_vwindow_above", "PageDown", (wildcard) => {
      let dY = self.refs.spreadsheet.getVisibleRows();
      self.refs.spreadsheet.shiftSelectionArea(dY, 0);
    });

    ShortcutUtils.addShortcut("grid", "grid_delete", "Del", (wildcard) => {
      let rng = Store.getActiveSelection();
      // console.log("deleting cells in range: " + JSON.stringify(rng));
      API.sendDeleteRequest(rng);
    });
    ShortcutUtils.addShortcut("grid", "grid_undo", "Ctrl+Z", (wildcard) => {
      API.sendUndoRequest();
    });
    ShortcutUtils.addShortcut("grid", "grid_redo", "Ctrl+Shift+Z", (wildcard) => {
      API.sendRedoRequest();
    });
    ShortcutUtils.addShortcut("grid", "chart", "F11", (wildcard) => {
      // TODO
    });
    // These shortcuts are annoying as fuck. TODO ask if they're necessary.

    // ShortcutUtils.addShortcut("grid", "select_row", "Shift+Space", (wildcard) => {
    //   let sel = Store.getActiveSelection();
    //   self.refs.spreadsheet.makeSelection({row: sel.row, col: 1, row2: sel.row, col2: Infinity});
    // });
    // ShortcutUtils.addShortcut("grid", "select_col", "Ctrl+Space", (wildcard) => {
    //   let sel = Store.getActiveSelection();
    //   self.refs.spreadsheet.makeSelection({row: 1, col: sel.col, row2: Infinity, col2: sel.col});
    // });
    ShortcutUtils.addShortcut("grid", "insert_row", "Ctrl+Shift+[", (wildcard) => {
      // TODO
    });
    ShortcutUtils.addShortcut("grid", "insert_col", "Ctrl+Shift+]", (wildcard) => {
      // TODO
    });
    ShortcutUtils.addShortcut("grid", "grid_outline_range", "Ctrl+Shift+5", (wildcard) => {
      // TODO
    });


    ShortcutUtils.addShortcut("grid", "copy_expression_above", "Ctrl+Shift+'", (wildcard) => {
      // TODO test
      let sel = Store.getActiveSelection(),
          cell = Store.getCellAtLoc(sel.col, sel.row-1);
      if (cell) {
        let xp = Converter.clientCellGetExpressionObj(cell).expression || "";
        self.setExpression(xp);
      } else self.setToast("No cell above.", "Error");
    });
    ShortcutUtils.addShortcut("grid", "copy_value_above", "Ctrl+'", (wildcard) => {
      // TODO test
      let sel = Store.getActiveSelection(),
          cell = Store.getCellAtLoc(sel.col, sel.row-1);
      if (cell) {
        let xp = Util.showValue(Converter.clientCellGetValueObj(cell)) || "";
        self.setExpression(xp);
      } else self.setToast("No cell above.", "Error");
    });

    // textbox shortcuts -------------------------------------------------------------------------------
    ShortcutUtils.addShortcut("textbox", "textbox_enter", "Enter", (wildcard) => {
      if (self.state.userIsTyping){
        let editorState = {
          exp: self._getRawEditor().getValue(),
          lang: self.state.language
        };
        self.handleEvalRequest(editorState, 1, 0);
      }
      else {
        //TODO: Navigate down
      }
    });

    // top level shortcuts -------------------------------------------------------------------------------
    ShortcutUtils.addShortcut("toplevel", "select_tab_right", "Ctrl+PageDown", (wildcard) => {
      //TODO
    });
    ShortcutUtils.addShortcut("toplevel", "select_tab_left", "Ctrl+PageUp", (wildcard) => {
      //TODO
    });

  }
}
