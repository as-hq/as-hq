import keyMirror from 'react/lib/keyMirror';

export default {
  host_ws: 'ws://localhost:5000',
  // event name triggered from store, listened to by views
  CHANGE_EVENT: 'change',

  scrollCacheX: 0,
  scrollCacheY: 0,

  numCols: 100000,
  numRows: 100000,

  // Each time you add an action, add it here... They should be past-tense
  ActionTypes: keyMirror({
    CELL_CHANGED: null,
    RANGE_CHANGED: null,
    SCROLLED: null,
    GOT_UPDATED_CELLS: null
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
    Delete: "Delete"
  }
};
