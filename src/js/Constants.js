
import keyMirror from 'keymirror';
import Environment from './Environment';

export const HOST_BASE_URL = 'localhost';
export const HOST_WS_PORT = 5000;
export const HOST_IP = '18.102.225.27';

export default Object.assign({
  HOST_WS_URL: 'ws://' + HOST_BASE_URL + ':' + HOST_WS_PORT,
  HOST_STATIC_URL: 'http://' + HOST_BASE_URL + ':8000',

  isDebug: false,

  // server config parameters
  isProduction: true,
  isRemote: true,
  promptUser: false,
  showConnectionLost: true,

  // event name triggered from store, listened to by views
  CHANGE_EVENT: 'change',

  LARGE_SEARCH_BOUND: 1000, // for searching for data boundaries, etc.

  scrollCacheX: 50,
  scrollCacheY: 50,

  numCols: 26,
  numRows: 1000,

  numVisibleCols: 38,
  numVisibleRows: 31,

  cellWidthPx: 100, // ignored; actually set in Render.js with this.config.minWidth = 100;
  cellHeightPx: 20,

  gridXOffset: 25, // margin between left edge of div and cells
  gridYOffset: 22, // '' top edge ''

  editorHeight: 60,
  codeBarHeight:60,
  topbarTotalHeight: 200, // height of everything except pane

  // Each time you add an action, add it here... They should be past-tense
  ActionTypes: keyMirror({
    GOT_FAILURE: null,
    CELL_CHANGED: null,
    RANGE_CHANGED: null,
    SCROLLED: null,
    GOT_UPDATED_CELLS: null,
    GOT_UNDO:null,
    GOT_REDO:null,
    GOT_SELECTION:null,
    FETCHED_CELLS:null,
    CLEARED: null,
    CLEARED_SHEET: null,
    GOT_UPDATED_WORKBOOKS: null,
    RECEIVED_ERROR: null,
    RECEIVED_SHEET: null,
    RECEIVED_WORKBOOK: null,
    DELETED_LOCS: null,
    REPL_LEFT:null,
    EVAL_HEADER_UPDATED:null,
    GOT_REPL_RESP:null,
    GOT_EVAL_HEADER_RESP:null,
    GOT_FIND:null,
    GOT_OPEN:null,
    GOT_NEW_WORKBOOKS: null,
    DELETED_WORKBOOKS: null,
    FIND_INCREMENTED:null,
    FIND_DECREMENTED:null,

    // Three-way data integration actions
    GRID_KEY_PRESSED: null,
    EDITOR_CHANGED:null,
    TEXTBOX_CHANGED:null,
    NORMAL_SEL_CHANGED:null,
    PARTIAL_REF_CHANGE_WITH_EDITOR:null,
    PARTIAL_REF_CHANGE_WITH_GRID:null,
    PARTIAL_REF_CHANGE_WITH_TEXTBOX:null,
    ESC_PRESSED:null,
    BACKEND_UPDATED_AND_CELLS_CHANGED: null
  }),

  ActionSources: keyMirror({
    SERVER_ACTION: null,
    VIEW_ACTION: null
  }),

  Languages: { 
    Excel: 'Excel', 
    Python: 'Python', 
    R: 'R', 
    SQL: 'SQL'
  },

  AceMode: {
    'Excel': 'text', 
    'Python': 'python', 
    'R': 'r', 
    'SQL': 'mysql'
  },

  ServerActions: {
    NoAction: "NoAction",
    Acknowledge: "Acknowledge",
    Evaluate: "Evaluate",
    Get: "Get",
    Delete: "Delete",
    Undo: "Undo",
    Redo: "Redo",
    Clear: "Clear",
    Copy: "Copy",
    Cut: "Cut",
    Repeat: "Repeat",
    UpdateWindow: "UpdateWindow",
    Open: "Open",
    Close: "Close",
    Import: "Import",
    Repl: "EvaluateRepl",
    EvalHeader: "EvaluateHeader",
    New: "New",
    Find: "Find",
    BugReport: "BugReport",
    ToggleProp: "ToggleProp",
    SetProp: "SetProp",
    JumpSelect: "JumpSelect",
    MutateSheet: "MutateSheet",
    Drag: "Drag",
    CondFormatting: "CondFormatting"
  },

  CursorPosition:{
    GRID: "GRID",
    TEXTBOX: "TEXTBOX",
    EDITOR: "EDITOR"
  },

  ClickType:{
    DOUBLE_CLICK: "DOUBLE_CLICK",
    CLICK: "CLICK"
  }

}, Environment);
