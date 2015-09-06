import keyMirror from 'react/lib/keyMirror';

export default {
  HOST_WS: 'ws://localhost:5000',

  // event name triggered from store, listened to by views
  CHANGE_EVENT: 'change',

  scrollCacheX: 200,
  scrollCacheY: 200,

  numCols: 100000,
  numRows: 100000,

  numVisibleCols: 38,
  numVisibleRows: 31,

  cellWidthPx: 50,
  cellHeightPx: 20,

  gridXOffset: 25, // margin between left edge of div and cells
  gridYOffset: 22, // '' top edge ''

  editorHeight: 100,
  topbarTotalHeight: 226, // height of everything except pane

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
    RECEIVED_ERROR: null,
    RECEIVED_SHEET: null,
    RECEIVED_WORKBOOK: null
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
    UpdateWindow: "UpdateWindow",
    Open: "Open",
    Close: "Close",
    Import: "Import"
  }
};
