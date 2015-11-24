import {logDebug} from './Logger';

import Constants from '../Constants';
import Store from '../stores/ASEvaluationStore';
import FindStore from '../stores/ASFindStore';
import SU from './ShortcutUtils';
import API from '../actions/ASApiActionCreators';
import Util from '../AS/Util';
import TC from '../AS/TypeConversions';
import KeyUtils from '../AS/KeyUtils';

import ExpStore from '../stores/ASExpStore';
import ExpActionCreator from '../actions/ASExpActionCreators';

export default {
  addShortcuts(evalPane) {
    // Should really technically be a part of ASEvaluationPane.jsx. Separated for convenience
    // though, hence self = evalPane.
    let self = evalPane;

    // common shortcuts -------------------------------------------------------------------------------

    SU.add('common', 'toggle_focus', 'F2', (wildcard) => {
      logDebug('F2 PRESSED ');
      Store.toggleFocusF2();
      self.refs.spreadsheet.refs.textbox.updateTextBox(ExpStore.getExpression());
      self.setFocus(Store.getFocus());
    });
    SU.add('common', 'new_sheet', 'Shift+F11', (wildcard) => {
      // TODO
    });
    SU.add('common', 'cell_eval', 'Ctrl+Enter', (wildcard) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: self.state.currentLanguage
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, null, null);
    });
    SU.add('common', 'cell_eval_up', 'Shift+Enter', (wildcard) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: self.state.currentLanguage
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, 0, -1);
    });
    SU.add('common', 'cell_eval_left', 'Shift+Tab', (wildcard) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: self.state.currentLanguage
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, -1, 0);
    });
    SU.add('common', 'cell_eval_arrayformula', 'Ctrl+Shift+Enter', (wildcard) => {
      // Always eval and stay put, but only sometimes add brackets
      let editorValue = self._getRawEditor().getValue();
      if (self.state.currentLanguage == Constants.Languages.Excel) {
        if (editorValue[0]==='=') {
          editorValue = '{' + editorValue + '}';
          self._getRawEditor().setValue(editorValue);
        }
      }
      let xpObj = {
        expression: editorValue,
        language: self.state.currentLanguage
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, 0, 0);
    });

    SU.add('common', 'set_language', 'Ctrl+1/2/3/4/5/6/7/8/9', (wildcard) => {
      switch(wildcard) {
          case '1':
            self.selectLanguage(Constants.Languages.Excel);
            break;
          case '2':
            self.selectLanguage(Constants.Languages.Python);
            break;
          case '3':
            self.selectLanguage(Constants.Languages.R);
            break;
          case '4':
            self.selectLanguage(Constants.Languages.SQL);
            break;
        }
    });
    SU.add('common', 'format_value', 'Ctrl+Shift+2/3/4/5/6', (wildcard) => {
      let dispType;
      // TODO other wildcards
      if (wildcard === '$') dispType = "Money";
      else if (wildcard === '%') dispType = "Percentage";
      API.setFormat(dispType, Store.getActiveSelection().range);
      self.refs.spreadsheet.repaint();
    });
    SU.add("common", "bold", "Ctrl+B", (wildcard) => {
      API.toggleProp("Bold", Store.getActiveSelection().range);
      self.refs.spreadsheet.repaint();
    });
    SU.add("common", "italic", "Ctrl+I", (wildcard) => {
      API.toggleProp("Italic", Store.getActiveSelection().range);
      self.refs.spreadsheet.repaint();
    });
    SU.add('common', 'toggle_repl', 'Alt+F11', (wildcard) => {
      self._toggleRepl();
    });
    SU.add('common', 'esc', 'Esc', (wildcard) => {
      logDebug('Esc pressed');
      ExpActionCreator.handleEscape();
      self.refs.spreadsheet.select(Store.getActiveSelection());
      Store.setClipboard(null, false);
      self.setState({focus: 'grid', showFindBar: false, userIsTyping: false});
      self.refs.spreadsheet.repaint(); // render immediately
    });

    SU.add('common', 'find', 'Ctrl+F', (wildcard) => {
      logDebug('Find pressed');
      self.setState({showFindBar:true,userIsTyping:false});
    });

    SU.add('common', 'insert_ref', 'Ctrl+J', () => {
      // Ctrl+J is a currently unassigned shortcut in Mac/Windows Excels
      // This lets users insert refs in Python
      logDebug('Inserting a ref');
      ExpStore.enableRefInsertionBypass();
    });

    // repl shortcuts -------------------------------------------------------------------------------
    SU.add('repl', 'repl_submit', ['Enter', 'Ctrl+Enter'], (wildcard) => {
      /* Preprocessing of repl value to get the 'last' part to send to server */
      let strs = self._replValue().split('>>>').slice(-1)[0].substring(1);
      let lines = strs.split('\n');
      let send = lines.map((l) => {
        if (l.substring(0,4) === '    ')
          return l.substring(4);
        else return l;
      }).join('\n');

      logDebug('SEND REPL: ' + JSON.stringify(send));
      let xpObj = {
        expression: send,
        language: self.state.replLanguage
      };
      // parse exp to get the last thing
      self.handleReplRequest(xpObj);
    });


    // editor shortcuts -------------------------------------------------------------------------------
    SU.add('common', 'toggle_reference', 'F4', (wildcard) => {
      let focus = Store.getFocus(),
          xp = ExpStore.getExpression();

      if (focus === 'grid') {
        let editor = self._getRawEditor(),
            sesh = editor.getSession(),
            cursor = editor.getCursorPosition(),
            range = Util.getExtendedWordRange(sesh, cursor.row, cursor.column), 
            sel = editor.selection;
        sel.setRange(range);
        let oldRef = editor.getSelectedText(),
            newRef = Util.toggleReference(oldRef);
        if (newRef !== null) {
          let newXp = xp.substring(0, xp.length - oldRef.length) + newRef;
          ExpActionCreator.handleGridChange(newXp);
        }
      } else if (focus === 'editor') {
        let editor = self._getRawEditor(),
            sesh = editor.getSession(),
            cursor = editor.getCursorPosition(),
            range = Util.getExtendedWordRange(sesh, cursor.row, cursor.column),
            sel = editor.selection;
        sel.setRange(range);
        let newRef = Util.toggleReference(editor.getSelectedText());
        if (newRef !== null) {
          sesh.replace(range, newRef);
          ExpActionCreator.handleEditorChange(editor.getValue());
        }
      } else if (focus === 'textbox') {
        let editor = self._getRawTextbox(),
            sesh = editor.getSession(),
            cursor = editor.getCursorPosition(),
            range = Util.getExtendedWordRange(sesh, cursor.row, cursor.column),
            sel = editor.selection;
        sel.setRange(range);
        let newRef = Util.toggleReference(editor.getSelectedText());
        if (newRef !== null) {
          sesh.replace(range, newRef);
          ExpActionCreator.handleTextBoxChange(editor.getValue());
        }
      }
    });



    // grid shortcuts -------------------------------------------------------------------------------
    // SU.add('grid', 'moveto_data_boundary', 'Ctrl+Up/Down/Left/Right', (wildcard) => {
    //    // -- For when backend-based jump is completed
    //    let {range, origin} = Store.getActiveSelection();
    //    API.jumpSelect(range, origin, false, wildcard);
    //  });
    //  SU.add('grid', 'moveto_data_boundary_extended', 'Ctrl+Shift+Up/Down/Left/Right', (wildcard) => {
    //    // -- For when backend-based jump is completed
    //    let {range, origin} = Store.getActiveSelection();
    //    API.jumpSelect(range, origin, true, wildcard);
    //  });
    SU.add('grid', 'moveto_data_boundary', 'Ctrl+Up/Down/Left/Right', (dir) => {
      // Needs to work even when you're selecting references while typing in the textbox
      // grid, which is why we're getting the spreadsheet's selection rather than the store's.
      // Might not be robust. (Alex 11/4)
      let oldInd = self.refs.spreadsheet.getSelectionArea().origin;
      let newInd = Store.getDataBoundary(oldInd, dir);
      self.refs.spreadsheet.select(TC.indexToSelection(newInd));
    });
    SU.add('grid', 'moveto_data_boundary_selected', 'Ctrl+Shift+Up/Down/Left/Right', (dir) => {
      // same comment as in moveto_data_boundary applies.
      // let oldSelection = Store.getActiveSelection();
      let oldSelection = self.refs.spreadsheet.getSelectionArea();
      let newSelection = Store.getDataBoundSelection(oldSelection, dir);
      self.refs.spreadsheet.select(newSelection);
    });
    SU.add('grid', 'grid_fill_down', 'Ctrl+D', (wildcard) => {
      let {tl, br} = Store.getActiveSelection().range;
      let copyFrom = TC.simpleToASRange({ tl: tl, br: {row: tl.row, col: br.col} }),
          copyTo = TC.simpleToASRange({ tl: {row: tl.row, col: tl.col},
                                               br: {row: br.row, col: tl.col} });
      API.copy(copyFrom, copyTo);
    });
    SU.add('grid', 'grid_fill_right', 'Ctrl+R', (wildcard) => {
      let {tl, br} = Store.getActiveSelection().range;
      let copyFrom = TC.simpleToASRange({ tl: tl, br: {row: br.row, col: tl.col} }),
          copyTo = TC.simpleToASRange({ tl: {row: tl.row, col: tl.col},
                                               br: {row: tl.row, col: br.col} });
      API.copy(copyFrom, copyTo);
    });
    SU.add('grid', 'grid_select_all', 'Ctrl+A', (wildcard) => {
      if (ExpStore.getUserIsTyping()) {
        self._getRawTextbox().selectAll();
      } else {
        let {origin} = Store.getActiveSelection();
        let range = self.refs.spreadsheet.getViewingWindow().range;
        self.refs.spreadsheet.select({origin: origin, range: range}, false);
      }
    });
    SU.add('grid,isTyping', 'grid_home_typing', ['Home', 'Ctrl+Home'], (wildcard) => {
      self.setFocus('textbox');
      self._getRawTextbox().navigateFileStart();
    });
    SU.add('grid,notTyping', 'grid_home', ['Home', 'Ctrl+Home'], (wildcard) => {
      let idx = {row: 1, col: 1};
      self.refs.spreadsheet.select(TC.indexToSelection(idx));
    });
    SU.add('grid,isTyping', 'grid_end_typing', 'End', (wildcard) => {
      self.setFocus('textbox');
      self._getRawTextbox().navigateFileEnd();
    });
    SU.add('grid', 'move_vwindow_above', 'PageUp', (wildcard) => {
      let dY = self.refs.spreadsheet.getVisibleRows();
      self.refs.spreadsheet.shiftSelectionArea(0, -dY);
    });
    SU.add('grid', 'move_vwindow_above', 'PageDown', (wildcard) => {
      let dY = self.refs.spreadsheet.getVisibleRows();
      self.refs.spreadsheet.shiftSelectionArea(0, dY);
    });
    SU.add('grid', 'grid_delete', 'Del/Backspace', (wildcard) => {
      let rng = Store.getActiveSelection().range;
      API.deleteRange(TC.simpleToASRange(rng));
    });
    SU.add('grid', 'grid_undo', 'Ctrl+Z', (wildcard) => {
      API.undo();
    });
    SU.add('grid', 'grid_redo', 'Ctrl+Shift+Z', (wildcard) => {
      API.redo();
    });
    SU.add('grid', 'grid_repeat_last_action', 'Ctrl+Y', (wildcard) => {
      let sel = Store.getActiveSelection();
      API.repeat(sel);
    });
    SU.add('grid', 'chart', 'F11', (wildcard) => {
      // TODO
    });
    SU.add('grid', 'select_row', 'Shift+Space', (wildcard) => {
      if (ExpStore.getUserIsTyping()) {
        logDebug("Grid key down going to AC");
        let newStr = ExpStore.getExpression() + ' ';
        ExpActionCreator.handleGridChange(newStr);
      } else {
        let {origin} = Store.getActiveSelection();
        self.refs.spreadsheet.select({range: {tl: {row: origin.row, col: 1}, br: {row: origin.row, col: Infinity}},
                                     origin: origin}, false);
      }
    });
    SU.add('grid,notTyping', 'select_col', 'Ctrl+Space', (wildcard) => {
      let {origin} = Store.getActiveSelection();
      self.refs.spreadsheet.select({range: {tl: {row: 1, col: origin.col}, br: {row: Infinity, col: origin.col}},
                                    origin: origin}, false);
    });
    SU.add('grid', 'insert_row', 'Ctrl+Shift+[', (wildcard) => {
      // TODO
    });
    SU.add('grid', 'insert_col', 'Ctrl+Shift+]', (wildcard) => {
      // TODO
    });
    SU.add('grid', 'grid_outline_range', 'Ctrl+Shift+5', (wildcard) => {
      // TODO
    });
    SU.add('grid', 'copy_expression_above', 'Ctrl+Shift+\'', (wildcard) => {
      // TODO test
      let {tl} = Store.getActiveSelection().range,
          cell = Store.getCell(tl.col, tl.row-1);
      if (cell) {
        let xp = cell.cellExpression.expression || '';
        self.setExpression(xp);
      } else self.setToast('No cell above.', 'Error');
    });
    SU.add('grid', 'copy_value_above', 'Ctrl+\'', (wildcard) => {
      // TODO test
      let tl = Store.getActiveSelection().range.tl,
          cell = Store.getCell(tl.col, tl.row-1);
      if (cell) {
        let xp = Util.showValue(cell.cellValue) || '';
        self.setExpression(xp);
      } else self.setToast('No cell above.', 'Error');
    });

    SU.add('grid', 'grid_enter', 'Enter', (wildcard) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: self.state.currentLanguage
      };
      self.handleEvalRequest(xpObj, 0, 1);
    });

    SU.add('grid', 'grid_eval_right', 'Tab', (wildcard) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: self.state.currentLanguage
      };
      self.handleEvalRequest(xpObj, 1, 0);
    });

    SU.add('grid', 'grid_mac_cut', 'Cmd+X', () => {
      evalPane.handleCopyTypeEventForGrid({
        preventDefault() { },
        stopPropagation() { },
        clipboardData: {
          setData() { }
        }
      }, true);
    });

    SU.add('grid', 'grid_mac_copy', 'Cmd+C', () => {
      evalPane.handleCopyTypeEventForGrid({
        preventDefault() { },
        stopPropagation() { },
        clipboardData: {
          setData() { }
        }
      }, false);
    });

    SU.add('grid', 'grid_mac_paste', 'Cmd+V', () => {
      evalPane.handlePasteEventForGrid({
        preventDefault() { },
        stopPropagation() { },
        clipboardData: {
          getData(x) { return 'alphasheets'; },
          types: ['text/html']
        }
      });
    });

    // textbox shortcuts -------------------------------------------------------------------------------
    SU.add('textbox', 'textbox_enter', 'Enter', (wildcard) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: self.state.currentLanguage
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, 0, 1);
    });

    SU.add('textbox', 'textbox_eval_right', 'Tab', (wildcard) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: self.state.currentLanguage
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, 1, 0);
    });

    // top level shortcuts -------------------------------------------------------------------------------
    SU.add('toplevel', 'select_tab_right', 'Ctrl+PageDown', (wildcard) => {
      //TODO
    });
    SU.add('toplevel', 'select_tab_left', 'Ctrl+PageUp', (wildcard) => {
      //TODO
    });

  }
}
