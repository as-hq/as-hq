/* @flow */

import Environment from './Environment';

function keyMirror<T>(obj: T): T {
  let ret: any = {};
  var key;
  if (!(obj instanceof Object && !Array.isArray(obj))) {
    throw new Error('keyMirror(...): Argument must be an object.');
  }
  for (key in obj) {
    if (obj.hasOwnProperty(key)) {
      ret[key] = key;
    }
  }
  return (ret: T);
}

const Constants = Object.assign({

  REMOTE_ROUTER_PORT: 10000,

  getRouterUrl(): string {
    return 'http://' + Constants.getRemoteHost() + ':' + Constants.REMOTE_ROUTER_PORT;
  },

  // network parameters
  getBackendUrl(protocol: string, port: string): string {
    // note: "procotol" is "ws", "http", etc.
    return protocol + '://' + Constants.getRemoteHost() + ':' + port;
  },

  getRemoteHost(): string {
    return Constants.isRemote ? Constants.REMOTE_HOST : 'localhost';
  },

  BACKEND_WS_PORT: '5000',
  BACKEND_STATIC_PORT: '8000',
  BACKEND_IMPORT_PORT: '9000',

  // auth parameters
  google_client_id: '347875438909-e81ep6ofitkq4deio3kagakpr5ujeh20.apps.googleusercontent.com',

  // server config parameters
  isDebug: false,
  isProduction: true,
  isRemote: true,
  promptUser: false,
  showConnectionLost: true,
  shouldReconnect: true,

  // event name triggered from store, listened to by views
  CHANGE_EVENT: 'change',

  LARGE_SEARCH_BOUND: 1000, // for searching for data boundaries, eU.Conversion.

  scrollCacheX: 50,
  scrollCacheY: 50,

  numCols: 26,
  numRows: 1000,

  numVisibleCols: 38,
  numVisibleRows: 31,

  numDecimalDigitsToShow: 10,

  cellWidthPx: 100, // ignored; actually set in Render.js with this.config.minWidth = 100;
  cellHeightPx: 20,

  gridXOffset: 25, // margin between left edge of div and cells
  gridYOffset: 22, // '' top edge ''

  // Each time you add an action, add it here... They should be past-tense
  ActionTypes: keyMirror({
    GOT_FAILURE: null,
    CELL_CHANGED: null,
    RANGE_CHANGED: null,
    SCROLLED: null,
    GOT_UNDO:null,
    GOT_REDO:null,
    GOT_SELECTION:null,
    GOT_UPDATED_CELLS:null,
    CLEARED: null,
    CLEARED_SHEET: null,
    GOT_UPDATED_WORKBOOKS: null,
    RECEIVED_ERROR: null,
    RECEIVED_SHEET: null,
    RECEIVED_WORKBOOK: null,
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
    BACKEND_UPDATED_AND_CELLS_CHANGED: null,
    EVAL_TRIED_TO_DECOUPLE: null,
    DROPDOWN_CLICKED: null,
    ADD_OVERLAY: null,
    SET_CONNECTING_STATE: null
  }),

  // We don't track the progress of these actions.
  UntimedActions: [
    'Initialize',
    'Open',
    'UpdateWindow',
    'ClearSheetServer',
    'BugReport',
    'Timeout',
    'ImportCSV'
  ],

  ActionSources: keyMirror({
    SERVER_ACTION: null,
    VIEW_ACTION: null
  }),

  CellProps: {
    Bold: { tag: 'Bold' },
    Italic: { tag: 'Italic' },
    Underline: { tag: 'Underline' }
  },

  Languages: {
    Excel: 'Excel',
    Python: 'Python',
    R: 'R',
    SQL: 'SQL'
  },

  HeaderLanguages: {
    Python: 'Python',
    R: 'R'
  },

  AceMode: {
    'Excel': 'text',
    'Python': 'python',
    'R': 'r',
    'SQL': 'mysql'
  },

  CursorPosition:{
    GRID: "GRID",
    TEXTBOX: "TEXTBOX",
    EDITOR: "EDITOR"
  },

  ClickType:{
    DOUBLE_CLICK: "DOUBLE_CLICK",
    CLICK: "CLICK"
  },

  ChartTypes: keyMirror({
    Line: null,
    Bar: null,
    Radar: null,
    PolarArea: null,
    Pie: null,
    Doughnut: null
  }),

  Colors: {"aliceblue":"#f0f8ff","antiquewhite":"#faebd7","aqua":"#00ffff","aquamarine":"#7fffd4","azure":"#f0ffff",
        "beige":"#f5f5dc","bisque":"#ffe4c4","black":"#000000","blanchedalmond":"#ffebcd","blue":"#0000ff","blueviolet":"#8a2be2","brown":"#a52a2a","burlywood":"#deb887",
        "cadetblue":"#5f9ea0","chartreuse":"#7fff00","chocolate":"#d2691e","coral":"#ff7f50","cornflowerblue":"#6495ed","cornsilk":"#fff8dc","crimson":"#dc143c","cyan":"#00ffff",
        "darkblue":"#00008b","darkcyan":"#008b8b","darkgoldenrod":"#b8860b","darkgray":"#a9a9a9","darkgreen":"#006400","darkkhaki":"#bdb76b","darkmagenta":"#8b008b","darkolivegreen":"#556b2f",
        "darkorange":"#ff8c00","darkorchid":"#9932cc","darkred":"#8b0000","darksalmon":"#e9967a","darkseagreen":"#8fbc8f","darkslateblue":"#483d8b","darkslategray":"#2f4f4f","darkturquoise":"#00ced1",
        "darkviolet":"#9400d3","deeppink":"#ff1493","deepskyblue":"#00bfff","dimgray":"#696969","dodgerblue":"#1e90ff",
        "firebrick":"#b22222","floralwhite":"#fffaf0","forestgreen":"#228b22","fuchsia":"#ff00ff",
        "gainsboro":"#dcdcdc","ghostwhite":"#f8f8ff","gold":"#ffd700","goldenrod":"#daa520","gray":"#808080","green":"#008000","greenyellow":"#adff2f",
        "honeydew":"#f0fff0","hotpink":"#ff69b4", "indianred ":"#cd5c5c","indigo":"#4b0082","ivory":"#fffff0","khaki":"#f0e68c",
        "lavender":"#e6e6fa","lavenderblush":"#fff0f5","lawngreen":"#7cfc00","lemonchiffon":"#fffacd","lightblue":"#add8e6","lightcoral":"#f08080","lightcyan":"#e0ffff","lightgoldenrodyellow":"#fafad2",
        "lightgrey":"#d3d3d3","lightgreen":"#90ee90","lightpink":"#ffb6c1","lightsalmon":"#ffa07a","lightseagreen":"#20b2aa","lightskyblue":"#87cefa","lightslategray":"#778899","lightsteelblue":"#b0c4de",
        "lightyellow":"#ffffe0","lime":"#00ff00","limegreen":"#32cd32","linen":"#faf0e6",
        "magenta":"#ff00ff","maroon":"#800000","mediumaquamarine":"#66cdaa","mediumblue":"#0000cd","mediumorchid":"#ba55d3","mediumpurple":"#9370d8","mediumseagreen":"#3cb371","mediumslateblue":"#7b68ee",
        "mediumspringgreen":"#00fa9a","mediumturquoise":"#48d1cc","mediumvioletred":"#c71585","midnightblue":"#191970","mintcream":"#f5fffa","mistyrose":"#ffe4e1","moccasin":"#ffe4b5",
        "navajowhite":"#ffdead","navy":"#000080","oldlace":"#fdf5e6","olive":"#808000","olivedrab":"#6b8e23","orange":"#ffa500","orangered":"#ff4500","orchid":"#da70d6",
        "palegoldenrod":"#eee8aa","palegreen":"#98fb98","paleturquoise":"#afeeee","palevioletred":"#d87093","papayawhip":"#ffefd5","peachpuff":"#ffdab9","peru":"#cd853f","pink":"#ffc0cb","plum":"#dda0dd","powderblue":"#b0e0e6","purple":"#800080",
        "red":"#ff0000","rosybrown":"#bc8f8f","royalblue":"#4169e1","saddlebrown":"#8b4513","salmon":"#fa8072","sandybrown":"#f4a460","seagreen":"#2e8b57","seashell":"#fff5ee","sienna":"#a0522d","silver":"#c0c0c0","skyblue":"#87ceeb","slateblue":"#6a5acd","slategray":"#708090","snow":"#fffafa","springgreen":"#00ff7f","steelblue":"#4682b4",
        "tan":"#d2b48c","teal":"#008080","thistle":"#d8bfd8","tomato":"#ff6347","turquoise":"#40e0d0",
        "violet":"#ee82ee","wheat":"#f5deb3","white":"#ffffff","whitesmoke":"#f5f5f5","yellow":"#ffff00","yellowgreen":"#9acd32"},

  DefaultColors: {
    TextColor: '#000000',
    FillColor: '#ffffff'
  }
}, Environment);

export default Constants;
