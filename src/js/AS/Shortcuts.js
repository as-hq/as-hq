import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import ShortcutUtils from './ShortcutUtils';
import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';

export default {
  addShortcuts(evalPane) {
    let self = evalPane;
    console.log("adding shortcuts!");

    // common shortcuts -------------------------------------------------------------------------------

    ShortcutUtils.addShortcut("common", "toggle_focus", "F2", (wildcard) => {self.toggleFocus()});
    ShortcutUtils.addShortcut("common", "cell_eval", ["Ctrl+Enter", "Command+Enter"], (wildcard) => {
      let editorState = {
        exp: self._getRawEditor().getValue(),
        lang: self.state.language
      };
      self.handleEvalRequest(editorState);
    });
    ShortcutUtils.addShortcut("common", "set_language", ["Ctrl+1/2/3/4/5/6/7/8/9", "Command+1/2/3/4/5/6/7/8/9"], (wildcard) => {
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
    ShortcutUtils.addShortcut("common", "format_value", "Ctrl+Shift+4/5", (wildcard) => {
      let tag;
      if (wildcard === '4')
        tag = {tag: "Money", contents: []};
      else
        tag = {tag: "Percentage", contents: []};
      let rng = Store.getActiveSelection();
      Store.addTag(tag, rng.col, rng.row);
      self.refs.hypergrid.repaint();
    });
    ShortcutUtils.addShortcut("common", "toggle_repl", "Alt+F11", (wildcard) => {
      // TODO
    });

    // repl shortcuts -------------------------------------------------------------------------------
    ShortcutUtils.addShortcut("repl", "repl_submit", ["Ctrl+Enter", "Command+Enter"], (wildcard) => {
      /* Preprocessing of repl value to get the "last" part to send to server */
      let strs = self._replValue().split(">>>").slice(-1)[0],
          idxs = Util.getIndicesOf("\n", strs),
          lines = strs.substring(idxs[0]).split("\n"),
          send = Util.removeEmptyLines(strs.substring(idxs[0]));
         
      console.log("SEND: " + send);

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
    ShortcutUtils.addShortcut("common", "esc", "Esc", (wildcard) => {
      let editor = self._getRawEditor();
      editor.setValue("");
      Store.setClipboard(null, false);
      self.setState({focus: "grid"});
      self.refs.spreadsheet.repaint(); // render immediately
    });

    // grid shortcuts -------------------------------------------------------------------------------
    ShortcutUtils.addShortcut("grid", "moveto_data_boundary", "Ctrl+Up/Down/Left/Right", (wildcard) => {
      switch(wildcard) {
        case "Up":
          break; // TODO
        case "Down":
          break; // TODO
        case "Left":
          break; // TODO
        case "Right":
          break; // TODO
      }
    });
    ShortcutUtils.addShortcut("grid", "copy", "Ctrl+C", (wildcard) => {
      let rng = Store.getActiveSelection();
      Store.setClipboard(rng, false);
      console.log("copying!");
      self.refs.spreadsheet.repaint(); // render immediately
    });
    ShortcutUtils.addShortcut("grid", "cut", "Ctrl+X", (wildcard) => {
      let rng = Store.getActiveSelection();
      Store.setClipboard(rng, true);
      self.refs.spreadsheet.repaint(); // render immediately
    });
    ShortcutUtils.addShortcut("grid", "paste", "Ctrl+V", (wildcard) => {
      let rng = Store.getActiveSelection();
      let clipboard = Store.getClipboard();
      if (clipboard.range)
        API.sendCopyRequest([clipboard.range, rng]);
      if (clipboard.isCut)
        API.sendDeleteRequest(clipboard.range);
      Store.setClipboard(null, false);
      self.refs.spreadsheet.repaint(); // render immediately
    });
    ShortcutUtils.addShortcut("grid", "grid_delete", "Del", (wildcard) => {
      let rng = Store.getActiveSelection();
      console.log("deleting cells in range: " + JSON.stringify(rng));
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
    ShortcutUtils.addShortcut("grid", "select_row", "Shift+Space", (wildcard) => {
      // TODO
    });
    ShortcutUtils.addShortcut("grid", "select_col", "Ctrl+Space", (wildcard) => {
      // TODO
    });
    ShortcutUtils.addShortcut("grid", "insert_row", "Ctrl+Shift+[", (wildcard) => {
      // TODO
    });
    ShortcutUtils.addShortcut("grid", "insert_col", "Ctrl+Shift+]", (wildcard) => {
      // TODO
    });
  }
}
