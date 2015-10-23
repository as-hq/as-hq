import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import SU from './ShortcutUtils';
import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';
import Converter from '../AS/Converter';

export default {
  addShortcuts(evalPane) {
    let self = evalPane;

    // common shortcuts -------------------------------------------------------------------------------

    SU.add("common", "toggle_focus", "F2", (wildcard) => {
      console.log("F2 PRESSED " + self.state.userIsTyping);
      self.setState({userIsTyping:!self.state.userIsTyping},function(){
        self.updateTextBox(self.state.userIsTyping);
      });
    });
    SU.add("common", "new_sheet", "Shift+F11", (wildcard) => {
      // TODO
    });
    SU.add("common", "cell_eval", "Ctrl+Enter", (wildcard) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: self.state.language
      };
      self.handleEvalRequest(xpObj, 0, 1);
    });
    SU.add("common", "cell_eval_arrayformula", "Ctrl+Shift+Enter", (wildcard) => {
      var editorValue = self._getRawEditor().getValue();
      console.log(self.state.language);
      if (self.state.language == Constants.Languages.Excel){
        editorValue = "{" + self._getRawEditor().getValue() + "}";
        self._getRawEditor().setValue(editorValue);
      }
      let xpObj = {
        expression: editorValue,
        language: self.state.language
      };
      self.handleEvalRequest(xpObj, 0, 1);
    });

    SU.add("common", "set_language", "Ctrl+1/2/3/4/5/6/7/8/9", (wildcard) => {
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
    SU.add("common", "format_value", "Ctrl+Shift+2/3/4/5/6", (wildcard) => {
      let tag;
      // TODO other wildcards
      if (wildcard === '4')
        tag = {tag: "Money", contents: []};
      else if (wildcard === '5')
        tag = {tag: "Percentage", contents: []};
      let {col, row} = Store.getActiveSelection().range.tl;
      Store.addTag(tag, col, row);
      self.refs.spreadsheet.repaint();
    });
    SU.add("common", "toggle_repl", "Alt+F11", (wildcard) => {
      self._toggleRepl();
    });
    SU.add("common", "esc", "Esc", (wildcard) => {
      console.log("Esc pressed");
      self.updateTextBox(false);
      Store.setClipboard(null, false);
      self.setState({focus: "grid",userIsTyping:false});
      self.refs.spreadsheet.repaint(); // render immediately
    });

    // repl shortcuts -------------------------------------------------------------------------------
    SU.add("repl", "repl_submit", "Ctrl+Enter", (wildcard) => {
      /* Preprocessing of repl value to get the "last" part to send to server */
      let strs = self._replValue().split(">>>").slice(-1)[0].substring(1);
      let lines = strs.split("\n");
      let send = lines.map((l) => {
        if (l.substring(0,4) === "    ")
          return l.substring(4);
        else return l;
      }).join("\n");

      console.log("SEND REPL: " + JSON.stringify(send));
      let xpObj = {
        expression: send,
        language: self.state.replLanguage.Server
      };
      // parse exp to get the last thing
      self.handleReplRequest(xpObj);
    });


    // editor shortcuts -------------------------------------------------------------------------------
    SU.add("editor", "toggle_reference", "F4", (wildcard) => {
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
    SU.add("grid", "moveto_data_boundary", "Ctrl+Up/Down/Left/Right", (wildcard) => {
      let newLoc = Store.getExtendedRange(wildcard, false);
      console.log("moving to: ", newLoc);
      self.refs.spreadsheet.makeSelection(newLoc, newLoc);
    });
    SU.add("grid", "moveto_data_boundary_selected", "Ctrl+Shift+Up/Down/Left/Right", (wildcard) => {
      let oldOrigin = Store.getActiveSelection().origin;
      let newLoc = Store.getExtendedRange(wildcard, true);
      self.refs.spreadsheet.makeSelection(newLoc, oldOrigin);
    });
    SU.add("grid", "grid_fill_down", "Ctrl+D", (wildcard) => {
      let {tl, br} = Store.getActiveSelection().range;
      let copyFrom = Converter.simpleToASRange({ tl: tl, br: tl }),
          copyTo = Converter.simpleToASRange({ tl: {row: tl.row+1, col: tl.col},
                                               br: {row: br.row, col: tl.col} });
      API.copy(copyFrom, copyTo);
    });
    SU.add("grid", "grid_fill_right", "Ctrl+R", (wildcard) => {
      let {tl, br} = Store.getActiveSelection().range;
      let copyFrom = Converter.simpleToASRange({ tl: tl, br: tl }),
          copyTo = Converter.simpleToASRange({ tl: {row: tl.row, col: tl.col+1},
                                               br: {row: tl.row, col: br.col} });
      API.copy(copyFrom, copyTo);
    });
    SU.add("grid", "grid_select_all", "Ctrl+A", (wildcard) => {
      self.refs.spreadsheet.makeSelection(self.refs.spreadsheet.getViewingWindowWithCache().range);
    });
    SU.add("grid", "grid_home", ["Home", "Ctrl+Home"], (wildcard) => {
      let idx = {row: 1, col: 1};
      self.refs.spreadsheet.makeSelection({ tl: idx, br: idx });
    });
    SU.add("grid", "grid_moveto_end_sheet", "Ctrl+End", (wildcard) => {
      //TODO
    });
    SU.add("grid", "move_vwindow_above", "PageUp", (wildcard) => {
      let dY = self.refs.spreadsheet.getVisibleRows();
      self.refs.spreadsheet.shiftSelectionArea(0, -dY);
    });
    SU.add("grid", "move_vwindow_above", "PageDown", (wildcard) => {
      let dY = self.refs.spreadsheet.getVisibleRows();
      self.refs.spreadsheet.shiftSelectionArea(0, dY);
    });

    SU.add("grid", "grid_delete", "Del", (wildcard) => {
      let rng = Store.getActiveSelection().range;
      API.deleteRange(Converter.simpleToASRange(rng));
    });
    SU.add("grid", "grid_undo", "Ctrl+Z", (wildcard) => {
      API.undo();
    });
    SU.add("grid", "grid_redo", "Ctrl+Shift+Z", (wildcard) => {
      API.redo();
    });
    SU.add("grid", "chart", "F11", (wildcard) => {
      // TODO
    });
    // These shortcuts are annoying as fuck. TODO ask if they're necessary.

    // SU.add("grid", "select_row", "Shift+Space", (wildcard) => {
    //   let sel = Store.getActiveSelection();
    //   self.refs.spreadsheet.makeSelection({row: sel.row, col: 1, row2: sel.row, col2: Infinity});
    // });
    // SU.add("grid", "select_col", "Ctrl+Space", (wildcard) => {
    //   let sel = Store.getActiveSelection();
    //   self.refs.spreadsheet.makeSelection({row: 1, col: sel.col, row2: Infinity, col2: sel.col});
    // });
    SU.add("grid", "insert_row", "Ctrl+Shift+[", (wildcard) => {
      // TODO
    });
    SU.add("grid", "insert_col", "Ctrl+Shift+]", (wildcard) => {
      // TODO
    });
    SU.add("grid", "grid_outline_range", "Ctrl+Shift+5", (wildcard) => {
      // TODO
    });


    SU.add("grid", "copy_expression_above", "Ctrl+Shift+'", (wildcard) => {
      // TODO test
      let tl = Store.getActiveSelection().range.tl,
          cell = Store.getCell(tl.col, tl.row-1);
      if (cell) {
        let xp = cell.cellExpression.expression || "";
        self.setExpression(xp);
      } else self.setToast("No cell above.", "Error");
    });
    SU.add("grid", "copy_value_above", "Ctrl+'", (wildcard) => {
      // TODO test
      let tl = Store.getActiveSelection().range.tl,
          cell = Store.getCell(tl.col, tl.row-1);
      if (cell) {
        let xp = Util.showValue(cell.cellValue) || "";
        self.setExpression(xp);
      } else self.setToast("No cell above.", "Error");
    });

    // textbox shortcuts -------------------------------------------------------------------------------
    SU.add("textbox", "textbox_enter", "Enter", (wildcard) => {
      if (self.state.userIsTyping){
        let xpObj = {
          expression: self._getRawEditor().getValue(),
          language: self.state.language
        };
        self.handleEvalRequest(xpObj, 0, 1);
      }
      else {
        // self.killTextbox();
        // self.refs.spreadsheet.shiftSelectionArea(0,1);
      }
    });

    // top level shortcuts -------------------------------------------------------------------------------
    SU.add("toplevel", "select_tab_right", "Ctrl+PageDown", (wildcard) => {
      //TODO
    });
    SU.add("toplevel", "select_tab_left", "Ctrl+PageUp", (wildcard) => {
      //TODO
    });

  }
}
