import keyMirror from 'react/lib/keyMirror';

export default {
  HOST_BASE_URL: 'localhost',
  HOST_WS: 'ws://' + 'localhost' + ':5000',
  HOST_STATIC_URL: 'http://' + 'localhost' + ':8000',

  // event name triggered from store, listened to by views
  CHANGE_EVENT: 'change',

  LARGE_SEARCH_BOUND: 1000, // for searching for data boundaries, etc.

  scrollCacheX: 50,
  scrollCacheY: 50,

  numCols: 100000,
  numRows: 100000,

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
    CELL_CHANGED: null,
    RANGE_CHANGED: null,
    SCROLLED: null,
    GOT_UPDATED_CELLS: null,
    GOT_UNDO:null,
    GOT_REDO:null,
    FETCHED_CELLS:null,
    CLEARED: null,
    GOT_UPDATED_WORKBOOKS: null,
    RECEIVED_ERROR: null,
    RECEIVED_SHEET: null,
    RECEIVED_WORKBOOK: null,
    DELETED_LOCS: null,
    REPL_LEFT:null,
    GOT_REPL_RESP:null,
    GOT_NEW_WORKBOOKS: null,
    DELETED_WORKBOOKS: null,
    FIND_INCREMENTED:null,
    FIND_DECREMENTED:null,

    // Three-way data integration actions
    GRID_KEY_PRESSED: null,
    EDITOR_CHANGED:null,
    TEXTBOX_CHANGED:null,
    NORMAL_SEL_CHANGED:null,
    PARTIAL_REF_FOUND:null
  }),

  ActionSources: keyMirror({
    SERVER_ACTION: null,
    VIEW_ACTION: null
  }),

  Languages: {
    Python: {
      Display: 'Python',
      Server: 'Python',
      Editor: 'python'
    },
    Excel: {
      Display: 'Excel',
      Server: 'Excel',
      Editor: 'python'
    },
    R: {
      Display: 'R',
      Server: 'R',
      Editor: 'r'
    },
    OCaml: {
      Display: 'OCaml',
      Server: 'OCaml',
      Editor: 'ocaml'
    },
    SQL: {
      Display: 'SQL',
      Server: 'SQL',
      Editor: 'mysql'
    },
    Java: {
      Display: 'Java',
      Server: 'Java',
      Editor: 'java'
    },
    CPP: {
      Display: 'C++',
      Server: 'CPP',
      Editor: 'c_cpp'
    }
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
    New: "New",
    Find:"Find",
    AddTags: "AddTags", RemoveTags: "RemoveTags"
  },

  xpChange: {
    FROM_GRID:0,
    FROM_EDITOR:1,
    SEL_CHNG:2,
    PARTIAL_REF_CHNG:3,
    FROM_TEXTBOX:4,
    NONE:5
  }
};
