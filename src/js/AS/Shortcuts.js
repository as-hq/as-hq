/* @flow */

import {logDebug} from './Logger';

import Constants from '../Constants';
import CellStore from '../stores/ASCellStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import SelectionStore from '../stores/ASSelectionStore';
import FindStore from '../stores/ASFindStore';
import API from '../actions/ASApiActionCreators';
import U from './Util';

import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

let {
  Shortcut: SU,
  Conversion: TC,
  Key: KeyUtils
} = U;

import ASEvalPane from '../components/ASEvaluationPane.jsx';

import ExpStore from '../stores/ASExpStore';
import ExpActionCreator from '../actions/ASExpActionCreators';

export default {
  addShortcuts(evalPane: ASEvalPane) {
    // Should really technically be a part of ASEvaluationPane.jsx. Separated for convenience
    // though, hence self = evalPane.
    let self = evalPane;

    // evalPane shortcuts -------------------------------------------------------------------------------
    SU.add('evalPane', 'toggle_focus', 'F2', (wildcard: string) => {
      logDebug('F2 PRESSED ');
      SheetStateStore.toggleFocusF2();
      self.getASSpreadsheet().refs.textbox.updateTextBox(ExpStore.getExpression());
      self.setFocus(SheetStateStore.getFocus());
    });
    SU.add('evalPane', 'new_sheet', 'Shift+F11', (wildcard: string) => {
      // TODO
    });
    SU.add('evalPane', 'cell_eval', 'Ctrl+Enter', (wildcard: string) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: ExpStore.getLanguage()
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, null, null);
    });
    SU.add('evalPane', 'cell_eval_up', 'Shift+Enter', (wildcard: string) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: ExpStore.getLanguage()
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, 0, -1);
    });
    SU.add('evalPane', 'cell_eval_left', 'Shift+Tab', (wildcard: string) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: ExpStore.getLanguage()
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, -1, 0);
    });
    SU.add('evalPane', 'cell_eval_arrayformula', 'Ctrl+Shift+Enter', (wildcard: string) => {
      // Always eval and stay put, but only sometimes add brackets
      let editorValue = self._getRawEditor().getValue();
      if (ExpStore.getLanguage() == Constants.Languages.Excel) {
        if (editorValue[0]==='=') {
          editorValue = '{' + editorValue + '}';
          self._getRawEditor().setValue(editorValue);
        }
      }
      let xpObj = {
        expression: editorValue,
        language: ExpStore.getLanguage()
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, 0, 0);
    });

    SU.add('evalPane', 'set_language', 'Ctrl+1/2/3/4', (wildcard: string) => {
      // Upon a shortcut, call toggle language, which will update the ExpStore's language and
      // inform the language dropdown in the toolbar of an update
      switch(wildcard) {
          case '1':
            ExpActionCreator.handleToggleLanguage(Constants.Languages.Excel);
            break;
          case '2':
            ExpActionCreator.handleToggleLanguage(Constants.Languages.Python);
            break;
          case '3':
            ExpActionCreator.handleToggleLanguage(Constants.Languages.R);
            break;
          case '4':
            ExpActionCreator.handleToggleLanguage(Constants.Languages.SQL);
            break;
        }
    });
    SU.add('evalPane', 'format_value', 'Ctrl+Shift+2/3/4/5/6', (wildcard: string) => {
      SelectionStore.withActiveSelection((sel) => {
        let formatType;
        // TODO other wildcards
        if (wildcard === '$') {
          formatType = "Money";
        }  if (wildcard === '%') {
          formatType = "Percentage";
        }
        if (formatType != null) {
          API.setFormat(formatType, sel.range);
          self.getASSpreadsheet().repaint();
        }
      });
    });
    SU.add("evalPane", "bold", "Ctrl+B", (wildcard: string) => {
      SelectionStore.withActiveSelection((sel) => {
        API.toggleProp({tag: "Bold", contents: []}, sel.range);
        self.getASSpreadsheet().repaint();
      });
    });
    SU.add("evalPane", "italic", "Ctrl+I", (wildcard: string) => {
      SelectionStore.withActiveSelection((sel) => {
        API.toggleProp({tag: "Italic", contents: []}, sel.range);
        self.getASSpreadsheet().repaint();
      });
    });

    SU.add('evalPane', 'esc', 'Esc', (wildcard: string) => {
      SelectionStore.withActiveSelection((sel) => {
        logDebug('Esc pressed');
        ExpActionCreator.handleEscape();
        self.getASSpreadsheet().select(sel);
        SheetStateStore.setClipboard(null, false);
        self.setState({focus: 'grid', showFindBar: false, userIsTyping: false});
        self.getASSpreadsheet().repaint(); // render immediately
      });
    });

    SU.add('evalPane', 'find', 'Ctrl+F', (wildcard: string) => {
      logDebug('Find pressed');
      self.setState({showFindBar:true, userIsTyping:false});
    });

    SU.add('evalPane', 'insert_ref', 'Ctrl+J', () => {
      // Ctrl+J is a currently unassigned shortcut in Mac/Windows Excels
      // This lets users insert refs in Python
      logDebug('Inserting a ref');
      ExpStore.enableRefInsertionBypass();
    });

    // repl shortcuts -------------------------------------------------------------------------------
    // NOT WORKING -- using header file right now

    // SU.add('repl', 'repl_submit', ['Enter', 'Ctrl+Enter'], (wildcard: string) => {
    //   /* Preprocessing of repl value to get the 'last' part to send to server */
    //   let strs = self._replValue().split('>>>').slice(-1)[0].substring(1);
    //   let lines = strs.split('\n');
    //   let send = lines.map((l) => {
    //     if (l.substring(0,4) === '    ')
    //       return l.substring(4);
    //     else return l;
    //   }).join('\n');

    //   logDebug('SEND REPL: ' + JSON.stringify(send));
    //   let xpObj = {
    //     expression: send,
    //     language: self.state.replLanguage
    //   };
    //   // parse exp to get the last thing
    //   self.handleReplRequest(xpObj);
    // });


    // editor shortcuts -------------------------------------------------------------------------------
    SU.add('evalPane', 'toggle_reference', 'F4', (wildcard: string) => {
      let focus = SheetStateStore.getFocus(),
          xp = ExpStore.getExpression();

      if (focus === 'grid') {
        let editor = self._getRawEditor(),
            sesh = editor.getSession(),
            cursor = editor.getCursorPosition(),
            range = U.Parsing.getExtendedWordRange(sesh, cursor.row, cursor.column),
            sel = editor.selection;
        sel.setRange(range);
        let oldRef = editor.getSelectedText(),
            newRef = U.Parsing.toggleReference(oldRef);
        if (newRef != null) {
          let newXp = xp.substring(0, xp.length - oldRef.length) + newRef;
          ExpActionCreator.handleGridChange(newXp);
        }
      } else if (focus === 'editor') {
        let editor = self._getRawEditor(),
            sesh = editor.getSession(),
            cursor = editor.getCursorPosition(),
            range = U.Parsing.getExtendedWordRange(sesh, cursor.row, cursor.column),
            sel = editor.selection;
        sel.setRange(range);
        let newRef = U.Parsing.toggleReference(editor.getSelectedText());
        if (!! newRef) {
          sesh.replace(range, newRef);
          ExpActionCreator.handleEditorChange(editor.getValue());
        }
      } else if (focus === 'textbox') {
        let editor = self._getRawTextbox(),
            sesh = editor.getSession(),
            cursor = editor.getCursorPosition(),
            range = U.Parsing.getExtendedWordRange(sesh, cursor.row, cursor.column),
            sel = editor.selection;
        sel.setRange(range);
        let newRef = U.Parsing.toggleReference(editor.getSelectedText());
        if (!! newRef) {
          sesh.replace(range, newRef);
          ExpActionCreator.handleTextBoxChange(editor.getValue());
        }
      }
    });



    // grid shortcuts -------------------------------------------------------------------------------
    // SU.add('grid', 'moveto_data_boundary', 'Ctrl+Up/Down/Left/Right', (wildcard: string) => {
    //    // -- For when backend-based jump is completed
    //    let {range, origin} = SelectionStore.getActiveSelection();
    //    API.jumpSelect(range, origin, false, wildcard: string);
    //  });
    //  SU.add('grid', 'moveto_data_boundary_extended', 'Ctrl+Shift+Up/Down/Left/Right', (wildcard: string) => {
    //    // -- For when backend-based jump is completed
    //    let {range, origin} = SelectionStore.getActiveSelection();
    //    API.jumpSelect(range, origin, true, wildcard: string);
    //  });
    SU.add('grid', 'moveto_data_boundary', 'Ctrl+Up/Down/Left/Right', (dir) => {
      // Needs to work even when you're selecting references while typing in the textbox
      // grid, which is why we're getting the spreadsheet's selection rather than the store's.
      // Might not be robust. (Alex 11/4)
      let oldInd = self.getASSpreadsheet().getSelectionArea().origin;
      let newInd = SheetStateStore.getDataBoundary(oldInd, dir);
      self.getASSpreadsheet().select(newInd.toSelection());
    });
    SU.add('grid', 'moveto_data_boundary_selected', 'Ctrl+Shift+Up/Down/Left/Right', (dir) => {
      // same comment as in moveto_data_boundary applies.
      // let oldSelection = SelectionStore.getActiveSelection();
      let oldSelection = self.getASSpreadsheet().getSelectionArea();
      let newSelection = SheetStateStore.getDataBoundSelection(oldSelection, dir);
      self.getASSpreadsheet().select(newSelection);
    });
    SU.add('grid', 'grid_fill_down', 'Ctrl+D', (wildcard: string) => {
      SelectionStore.withActiveSelection((sel) => {
        API.copy(sel.range.getTopRow(), sel.range);
      });
    });
    SU.add('grid', 'grid_fill_right', 'Ctrl+R', (wildcard: string) => {
      SelectionStore.withActiveSelection((sel) => {
        API.copy(sel.range.getLeftColumn(), sel.range);
      });
    });
    SU.add('grid', 'grid_select_all', 'Ctrl+A', (wildcard: string) => {
      if (ExpStore.getUserIsTyping()) {
        self._getRawTextbox().selectAll();
      } else {
        SelectionStore.withActiveSelection((sel) => {
          let {origin} = sel;
          let range = self.getASSpreadsheet().getViewingWindow();
          self.getASSpreadsheet().select(
            ASSelection.fromASLocations({
              origin: origin,
              range: range
            }),
          false);
        });
      }
    });
    SU.add('grid,isTyping', 'grid_home_typing', ['Home', 'Ctrl+Home'], (wildcard: string) => {
      self.setFocus('textbox');
      self._getRawTextbox().navigateFileStart();
    });
    SU.add('grid,notTyping', 'grid_home', ['Home', 'Ctrl+Home'], (wildcard: string) => {
      self.getASSpreadsheet().select(ASSelection.defaultSelection());
    });
    SU.add('grid,isTyping', 'grid_end_typing', 'End', (wildcard: string) => {
      self.setFocus('textbox');
      self._getRawTextbox().navigateFileEnd();
    });
    SU.add('grid', 'move_vwindow_above', 'PageUp', (wildcard: string) => {
      let dY = self.getASSpreadsheet().getVisibleRows();
      self.getASSpreadsheet().shiftSelectionArea({ dc: 0, dr: -dY });
    });
    SU.add('grid', 'move_vwindow_above', 'PageDown', (wildcard: string) => {
      let dY = self.getASSpreadsheet().getVisibleRows();
      self.getASSpreadsheet().shiftSelectionArea({ dc: 0, dr: dY });
    });
    SU.add('grid', 'grid_delete', 'Del/Backspace', (wildcard: string) => {
      SelectionStore.withActiveSelection((sel) => {
        let rng = sel.range;
        API.deleteRange(rng);
      });
    });
    SU.add('grid', 'grid_undo', 'Ctrl+Z', (wildcard: string) => {
      API.undo();
    });
    SU.add('grid', 'grid_redo', 'Ctrl+Shift+Z', (wildcard: string) => {
      API.redo();
    });
    SU.add('grid', 'grid_repeat_last_action', 'Ctrl+Y', (wildcard: string) => {
      SelectionStore.withActiveSelection((sel) => {
        API.repeat(sel);
      });
    });
    SU.add('grid', 'chart', 'F11', (wildcard: string) => {
      // TODO
    });
    SU.add('grid', 'select_row', 'Shift+Space', (wildcard: string) => {
      if (ExpStore.getUserIsTyping()) {
        logDebug("Grid key down going to AC");
        let oldStr = ExpStore.getExpression(),
            editor = self.getASSpreadsheet().refs.textbox.editor,
           [newStr, newPos] = KeyUtils.modifyTextboxForKey(KeyUtils.mockedKeyboardEvent(32),
                                                           true, null,
                                                           oldStr, editor);
        ExpActionCreator.handleGridChange(newStr, newPos);
      } else {
        SelectionStore.withActiveSelection((sel) => {
          let {origin} = sel;
          self.getASSpreadsheet().select(ASSelection.fromASLocations({
            range: origin.getRowRange(),
            origin: origin
          }));
        });
      }
    });
    SU.add('grid,notTyping', 'select_col', 'Ctrl+Space', (wildcard: string) => {
      SelectionStore.withActiveSelection((sel) => {
        let {origin} = sel;
        self.getASSpreadsheet().select(ASSelection.fromASLocations({
          range: origin.getColumnRange(),
          origin: origin
        }));
      });
    });
    SU.add('grid', 'insert_row', 'Ctrl+Shift+[', (wildcard: string) => {
      // TODO
    });
    SU.add('grid', 'insert_col', 'Ctrl+Shift+]', (wildcard: string) => {
      // TODO
    });
    SU.add('grid', 'grid_outline_range', 'Ctrl+Shift+5', (wildcard: string) => {
      // TODO
    });
    SU.add('grid', 'copy_expression_above', 'Ctrl+Shift+\'', (wildcard: string) => {
      // TODO test
      SelectionStore.withActiveSelection((sel) => {
        let {tl} = sel.range,
            cell = CellStore.getCell(tl.above());
        if (cell) {
          let xp = cell.expression.expression || '';
          ExpActionCreator.handleEditorChange(xp);
        } else self.setToast('No cell above.', 'Error');
      });
    });
    SU.add('grid', 'copy_value_above', 'Ctrl+\'', (wildcard: string) => {
      // TODO test
      SelectionStore.withActiveSelection((sel) => {
        let {tl} = sel.range,
            cell = CellStore.getCell(tl.above());
        if (cell) {
          let xp = U.Render.showValue(cell.value) || '';
          ExpActionCreator.handleEditorChange(xp);
          let xpObj = {
            expression: self._getRawEditor().getValue(),
            language: ExpStore.getLanguage()
          };
          self.setFocus('grid');
          self.handleEvalRequest(xpObj, null, null);
        } else self.setToast('No cell above.', 'Error');
      });
    });

    SU.add('grid', 'grid_enter', 'Enter', (wildcard: string) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: ExpStore.getLanguage()
      };
      self.handleEvalRequest(xpObj, 0, 1);
    });

    SU.add('grid', 'grid_eval_right', 'Tab', (wildcard: string) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: ExpStore.getLanguage()
      };
      self.handleEvalRequest(xpObj, 1, 0);
    });

    SU.add('grid', 'grid_mac_cut', 'Cmd+X', () => {
      evalPane.handleCopyTypeEventForGrid({
        preventDefault() { },
        stopPropagation() { },
        persist() { },
        clipboardData: {
          setData() { }
        }
      }, true);
    });

    SU.add('grid', 'grid_mac_copy', 'Cmd+C', () => {
      evalPane.handleCopyTypeEventForGrid({
        preventDefault() { },
        stopPropagation() { },
        persist() { },
        clipboardData: {
          setData() { }
        }
      }, false);
    });

    SU.add('grid', 'grid_mac_paste', 'Cmd+V', () => {
      evalPane.handlePasteEventForGrid({
        preventDefault() { },
        stopPropagation() { },
        persist() { },
        clipboardData: {
          getData(x) { return 'alphasheets'; },
          types: ['text/html']
        }
      });
    });

    // textbox shortcuts -------------------------------------------------------------------------------
    SU.add('textbox', 'textbox_enter', 'Enter', (wildcard: string) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: ExpStore.getLanguage()
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, 0, 1);
    });

    SU.add('textbox', 'textbox_eval_right', 'Tab', (wildcard: string) => {
      let xpObj = {
        expression: self._getRawEditor().getValue(),
        language: ExpStore.getLanguage()
      };
      self.setFocus('grid');
      self.handleEvalRequest(xpObj, 1, 0);
    });

    // eval header shortcuts -------------------------------------------------------------------------------
    SU.add('evalHeader', 'save', 'Ctrl+S', (wildcard: string) => {
      self.refs.evalHeader.saveAndEval();
    });

    // top level shortcuts -------------------------------------------------------------------------------
    SU.add('toplevel', 'select_tab_right', 'Ctrl+PageDown', (wildcard) => {
      //TODO
    });
    SU.add('toplevel', 'select_tab_left', 'Ctrl+PageUp', (wildcard) => {
      //TODO
    });

    SU.add('toplevel', 'toggle_header', 'Alt+H', (wildcard: string) => {
      self.toggleEvalHeader();
    });

  }
}
