/* @flow */

import type {
  ShortcutTarget,
  ShortcutRoot,
  ShortcutGroup,
  ASShortcut,
  ASKeyCombination,
  ASKeyModifier,
  ASKeyProperty
} from '../types/Keyboard';

import type {
  Callback
} from '../types/Base';

// $FlowFixMe
import invariant from 'invariant';
import {logDebug} from './Logger';

import Constants from '../Constants';
import CellStore from '../stores/ASCellStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import GridStore from '../stores/ASGridStore';
import HeaderStore from '../stores/ASHeaderStore';
import FindStore from '../stores/ASFindStore';
import ExpressionStore from '../stores/ASExpressionStore';

import API from '../actions/ASApiActionCreators';
// these are ACTUAL actions, and will be transitioned to from ASApiActionCreators
import APIActions from '../actions/APIActionCreators';
import DialogActions from '../actions/DialogActionCreators';
import HeaderActions from '../actions/ASHeaderActionCreators';
import GridActions from '../actions/ASGridActionCreators';
import ConfigActions from '../actions/ASConfigActionCreators';
import NotificationActions from '../actions/ASNotificationActionCreators';
import ClipboardActions from '../actions/ASClipboardActionCreators';

import U from './Util';

import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

import FocusStore from '../stores/ASFocusStore';
import FocusActions from '../actions/ASFocusActionCreators';
import ExpressionActions from '../actions/ASExpressionActionCreators';

let {
  Shortcut: SU,
  Conversion: TC,
  Key: KeyUtils
} = U;

// ****************************************************************************
// Shortcut actions & storage

const _shortcuts: {[key: ShortcutTarget]: Array<ASKeyCombination>} = {
  grid: [],
  editor: [],
  textbox: [],
  evalpane: [],
  header: [],
  toplevel: []
};

// a shortcut tree, stored as paths from leaves to the root ('toplevel')
const shortcutTree: {[key: ShortcutRoot]: Array<ShortcutGroup>} = {
  editor: ['evalpane', 'toplevel'],
  textbox: ['evalpane', 'toplevel'],
  grid: ['evalpane', 'toplevel'],
  header: ['toplevel'],
};

export const actions = {
  installAll() {
    installAllShortcuts();
  },

  install(target: ShortcutTarget, name: string, keys: any, cb: Callback<string>) {
    const shortcuts = SU.createConfiguration(target, name, keys, cb);
    shortcuts.forEach(shortcut => {
      _shortcuts[shortcut.set].push(shortcut);
    });
  },

  try(e: SyntheticKeyboardEvent, target: ShortcutRoot) {
    const set = _shortcuts[target];
    invariant(set, 'Cannot execute shortcut against unknown target: ' + target);

    // try executing the shortcut
    if (! SU.tryShortcut(e, set)) {
      for (const group of shortcutTree[target]) {
        if (SU.tryShortcut(e, _shortcuts[group])) return;
      }
    }
  }
}

// ****************************************************************************
// Install shortcuts

const { install } = actions;

function installAllShortcuts() {

// evalpane shortcuts -------------------------------------------------------------------------------
  install('evalpane', 'toggle_focus', 'F2', (wildcard: string) => {
    FocusActions.toggleFocusF2();
  });
  install('evalpane', 'new_sheet', 'Shift+F11', (wildcard: string) => {
    // TODO
  });
  install('evalpane', 'cell_eval', 'Ctrl+Enter', (wildcard: string) => {
    APIActions.evaluate({dX: 0, dY: 0});
  });
  install('evalpane', 'cell_eval_arrayformula', 'Ctrl+Shift+Enter', (wildcard: string) => {
    let expression = ExpressionStore.getExpression();
    if (ExpressionStore.getLanguage() === 'Excel') {
      if (expression[0] === '=') {
        expression = '{' + expression + '}';
      }
    }
    APIActions.evaluate({dX: 0, dY: 0}, expression);
  });
  install('evalpane', 'format_value', 'Ctrl+Shift+2|3|4|5|6', (wildcard: string) => {
    const sel = GridStore.getActiveSelection();
    let formatType;
    // TODO other wildcards
    if (wildcard === '$') {
      formatType = "Money";
    }  if (wildcard === '%') {
      formatType = "Percentage";
    }
    if (formatType != null) {
      API.setFormat(formatType, sel.range);
      GridActions.repaint();
    }
  });
  install("evalpane", "bold", "Ctrl+B", (wildcard: string) => {
    const sel = GridStore.getActiveSelection();
    API.toggleProp({tag: "Bold", contents: []}, sel.range);
    GridActions.repaint();
  });
  install("evalpane", "italic", "Ctrl+I", (wildcard: string) => {
    const sel = GridStore.getActiveSelection();
    API.toggleProp({tag: "Italic", contents: []}, sel.range);
    GridActions.repaint();
  });

  install('evalpane', 'esc', 'Esc', (wildcard: string) => {
    ExpressionActions.stopEditing();
  });

  install('evalpane', 'find', 'Ctrl+F', (wildcard: string) => {
    ConfigActions.openFindBar();
  });

  // Ctrl+J is a currently unassigned shortcut in Mac/Windows Excels
  // This lets users insert refs in Python
  install('evalpane', 'insert_ref', 'Ctrl+J', () => {
    // TODO fix
    // ExpressionActions.forceRefInsertion();
  });

  install('evalpane', 'toggle_reference', 'F4', (wildcard: string) => {
    ExpressionActions.toggleReference();
  });

  // error, header, and cell pane stuff
  install('evalpane', 'toggle_error_pane', 'Ctrl+Alt+E', (wildcard: string) => {
    ConfigActions.toggleBottomPane('errors');
  });

  install('evalpane', 'toggle_cell_pane', 'Ctrl+Alt+O', (wildcard: string) => {
    ConfigActions.toggleBottomPane('cell_output');
  });

  install('evalpane', 'toggle_header_pane', 'Ctrl+Alt+H', (wildcard: string) => {
    ConfigActions.toggleBottomPane('header_output');
  });

  // grid shortcuts -------------------------------------------------------------------------------

  install('grid', 'start_editing', ['Enter', 'Shift+Enter'], () => {
    ExpressionActions.startEditing(t => t, true);
  });

  install('grid', 'conditional_formatting', 'Ctrl+Shift+F', () => {
    DialogActions.openCondFormattingDialog();
  });

  install('grid', 'moveto_data_boundary', 'Ctrl+Up|Down|Left|Right', (dir) => {
    const { origin } = GridStore.getActiveSelection();
    const newOrigin = SheetStateStore.getDataBoundary(origin, dir);
    GridActions.select(newOrigin.toSelection());
  });
  install('grid', 'moveto_data_boundary_selected', 'Ctrl+Shift+Up|Down|Left|Right', (dir) => {
    const selection = GridStore.getActiveSelection();
    const newSelection = SheetStateStore.getDataBoundSelection(selection, dir);
    GridActions.select(newSelection);
  });
  install('grid', 'grid_fill_down', 'Ctrl+D', (wildcard: string) => {
    const {range} = GridStore.getActiveSelection();
    API.copy(range.getTopRow(), range);
  });
  install('grid', 'grid_fill_right', 'Ctrl+R', (wildcard: string) => {
    const {range} = GridStore.getActiveSelection();
    API.copy(range.getLeftColumn(), range);
  });
  install('grid', 'grid_select_all', 'Ctrl+A', (wildcard: string) => {
    const {origin} = GridStore.getActiveSelection();
    const range = GridStore.getViewingWindow();
    GridActions.select(
      ASSelection.fromASLocations({origin, range})
    );
  });
  install('grid', 'grid_home', ['Home', 'Ctrl+Home'], (wildcard: string) => {
    GridActions.select(ASSelection.defaultSelection());
  });
  install('grid', 'move_vwindow_above', 'PageUp', (wildcard: string) => {
    const {height} = GridStore.getDimensions();
    GridActions.scrollBy({dX: 0, dY: -1 * height});
    GridActions.shiftSelection({dX: 0, dY: -1 * height});
  });
  install('grid', 'move_vwindow_above', 'PageDown', (wildcard: string) => {
    const {height} = GridStore.getDimensions();
    GridActions.scrollBy({dX: 0, dY: height});
    GridActions.shiftSelection({dX: 0, dY: height});
  });
  install('grid', 'grid_delete', 'Del|Backspace', (wildcard: string) => {
    const {range} = GridStore.getActiveSelection();
    API.deleteRange(range);
  });
  install('grid', 'grid_undo', 'Ctrl+Z', (wildcard: string) => {
    API.undo();
  });
  install('grid', 'grid_redo', 'Ctrl+Shift+Z', (wildcard: string) => {
    API.redo();
  });
  install('grid', 'grid_repeat_last_action', 'Ctrl+Y', (wildcard: string) => {
    const sel = GridStore.getActiveSelection();
    API.repeat(sel);
  });
  install('grid', 'chart', 'F11', (wildcard: string) => {
    // TODO
  });
  install('grid', 'select_row', 'Shift+Space', (wildcard: string) => {
    const {origin: {col, row}} = GridStore.getActiveSelection();
    const selection = new ASSelection({
      origin: {col, row},
      range: {
        tl: {col: 0, row},
        br: {col: Infinity, row}
      }
    });
    GridActions.select(selection, false);
  });
  install('grid', 'select_col', 'Ctrl+Space', (wildcard: string) => {
    const {origin: {col, row}} = GridStore.getActiveSelection();
    const selection = new ASSelection({
      origin: {col, row},
      range: {
        tl: {col, row: 0},
        br: {col, row: Infinity}
      }
    });
    GridActions.select(selection, false);
  });
  install('grid', 'insert_row', 'Ctrl+Shift+[', (wildcard: string) => {
    // TODO
  });
  install('grid', 'insert_col', 'Ctrl+Shift+]', (wildcard: string) => {
    // TODO
  });
  install('grid', 'grid_outline_range', 'Ctrl+Shift+5', (wildcard: string) => {
    // TODO
  });
  install('grid', 'copy_expression_above', 'Ctrl+Shift+\'', (wildcard: string) => {
    const {range} = GridStore.getActiveSelection();
    const cell = CellStore.getCell(range.tl.above());
    if (!! cell) {
      ExpressionActions.setExpression(cell.expression.expression);
    }
  });
  install('grid', 'copy_value_above', 'Ctrl+\'', (wildcard: string) => {
    // TODO test
    const {range} = GridStore.getActiveSelection();
    const cell = CellStore.getCell(range.tl.above());
    if (cell) {
      const expression = U.Render.showValue(cell.value) || '';
      APIActions.evaluate({dX: 0, dY: 0}, expression.toString());
    } else self.setToast('No cell above.', 'Error');
  });

  install('grid', 'grid_tab', 'Tab', (wildcard: string) => {
    GridActions.shiftSelection({dX: 1, dY: 0}, false);
  });
  install('grid', 'grid_shift_tab', 'Shift+Tab', (wildcard: string) => {
    GridActions.shiftSelection({dX: -1, dY: 0}, false);
  });
  install('grid', 'grid_mac_cut', 'Cmd+X', () => {
    ClipboardActions.cut({
      preventDefault() { },
      stopPropagation() { },
      persist() { },
      clipboardData: {
        setData() { }
      }
    });
  });

  install('grid', 'grid_mac_copy', 'Cmd+C', () => {
    ClipboardActions.copy({
      preventDefault() { },
      stopPropagation() { },
      persist() { },
      clipboardData: {
        setData() { }
      }
    });
  });

  install('grid', 'grid_mac_paste', 'Cmd+V', () => {
    ClipboardActions.paste({
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
  install('textbox', 'textbox_eval_down', 'Enter', (wildcard: string) => {
    APIActions.evaluate({dX: 0, dY: 1});
  });

  install('textbox', 'textbox_eval_up', 'Shift+Enter', (wildcard: string) => {
    APIActions.evaluate({dX: 0, dY: -1});
  });

  install('textbox', 'textbox_eval_right', 'Tab', (wildcard: string) => {
    APIActions.evaluate({dX: 1, dY: 0});
  });

  install('textbox', 'textbox_eval_left', 'Shift+Tab', (wildcard: string) => {
    APIActions.evaluate({dX: -1, dY: 0});
  });


  // header shortcuts -------------------------------------------------------------------------------
  install('header', 'save', 'Ctrl+S', (wildcard: string) => {
    NotificationActions.addNotification({
      title: 'Evaluated!',
      level: 'success',
      autoDismiss: 1
    });
    const expression = HeaderStore.getCurrentExpression();
    const language = HeaderStore.getCurrentLanguage();
    API.evaluateHeader(expression, language);
  });

  // top level shortcuts -------------------------------------------------------------------------------
  install('toplevel', 'select_tab_right', 'Ctrl+PageDown', (wildcard) => {
    //TODO
  });
  install('toplevel', 'select_tab_left', 'Ctrl+PageUp', (wildcard) => {
    //TODO
  });

  install('toplevel', 'set_language', 'Ctrl+1|2|3|4', (wildcard: string) => {
    // Upon a shortcut, call toggle language, which will update the ExpStore's language and
    // inform the language dropdown in the toolbar of an update
    let language;
    switch(wildcard) {
        case '1':
          language = 'Excel';
          break;
        case '2':
          language = 'Python';
          break;
        case '3':
          language = 'R';
          break;
        case '4':
          language = 'SQL';
          break;
        default:
          language = ExpressionStore.getLanguage();
          break;
      }
    ExpressionActions.setLanguage(language);
  });

  install('toplevel', 'toggle_header', 'Alt+H', (wildcard: string) => {
    ConfigActions.toggleHeader();
  });
}
