/* @flow */

import type {
  ASExecError,
  ASExcelExecError
} from './Errors';

import type { 
  SheetUpdate, 
  CellUpdate, 
  BarUpdate, 
  CondFormatRuleUpdate
} from './Updates';

import type { 
  Bar, 
  BarIndex, 
  BarType, 
  BarProp
} from './Bar';

import type {
  NakedRange,
  ASLocation,
  ASRange,
  ASSelection,
  ASLanguage,
  ASIndex,
  ASSheet,
  ASValue,
  ASCompositeValue,
  ASExpression,
  ASWorkbook,
  ASCellProp,
  ASCell,
  FormatType
} from './Eval';

import type {
  ASViewingWindow
} from './State';

import type {
  ASUserId
} from './User';

export type Direction = 'Down' | 'Up' | 'Left' | 'Right';

export type ASBackendDirection = 'DDown' | 'DUp' | 'DLeft' | 'DRight';

export type QueryList = 'Sheets' | 'Workbooks' | 'WorkbookSheets';

export type InsertCol = {
  insertColNum: number;
};

export type InsertRow = {
  insertRowNum: number;
};

export type DeleteCol = {
  deleteColNum: number;
};

export type DeleteRow = {
  deleteRowNum: number;
};

export type DragCol = {
  oldColNum: number;
  newColNum: number;
};

export type DragRow = {
  oldRowNum: number;
  newRowNum: number;
};

export type MutateType =
  InsertCol
  | InsertRow
  | DeleteCol
  | DeleteRow
  | DragCol
  | DragRow;

export type ASInitConnection = {
  tag: 'ASInitConnection';
  connUserId: ASUserId;
  connSheetId: string;
};

export type PayloadN = {
  tag: 'PayloadN';
};

export type PayloadCL = {
  tag: 'PayloadCL';
  contents: Array<ASCell>;
};

export type PayloadLL = {
  tag: 'PayloadLL';
  contents: Array<ASIndex>;
};

export type PayloadSS = {
  tag: 'PayloadSS';
  contents: Array<ASSheet>;
};

export type PayloadWBS = {
  tag: 'PayloadWBS';
  contents: Array<ASWorkbook>;
};

export type PayloadWorkbookSheets = {
  tag: 'PayloadWorkbookSheets';
  contents: Array<ASBackendWorkbookSheet>;
};

export type PayloadSelection = {
  tag: 'PayloadSelection';
  selectionRange: ASRange;
  selectionOrigin: ASIndex;
};

export type PayloadJump = {
  tag: 'PayloadJump';
  jumpRange: ASRange;
  jumpOrigin: ASIndex;
  isShifted: boolean;
  jumpDirection: ASBackendDirection;
};

export type PayloadPaste = {
  tag: 'PayloadPaste';
  copyRange: ASRange;
  copyTo: ASRange;
};

export type PayloadProp = {
  tag: 'PayloadTag';
  prop: ASCellProp;
  tagRange: ASRange;
};

export type PayloadText = {
  tag: 'PayloadText';
  text: String;
};

export type PayloadDrag = {
  tag: 'PayloadDrag';
  initialRange: ASRange;
  dragRange: ASRange;
};

export type PayloadInit = {
  tag: 'PayloadInit';
  contents: ASInitConnection;
};

export type PayloadOpen = {
  tag: 'PayloadOpen';
  initHeaderExpressions: Array<ASExpression>;
  initSheetUpdate: SheetUpdate;
};

export type PayloadR = {
  tag: 'PayloadR';
  contents: ASRange;
};

export type PayloadS = {
  tag: 'PayloadS';
  contents: ASSheet;
};

export type PayloadWB = {
  tag: 'PayloadWB';
  contents: ASWorkbook;
};

export type PayloadW = {
  tag: 'PayloadW';
  contents: ASViewingWindow;
};

export type PayloadU = {
  tag: 'PayloadU';
  contents: ASUserId;
};

export type PayloadE = {
  tag: 'PayloadE';
  contents: ASExecError;
};

export type PayloadXp = {
  tag: 'PayloadXp';
  contents: ASExpression;
};

export type PayloadList = {
  tag: 'PayloadList';
  contents: QueryList;
};

export type PayloadMutate = {
  tag: 'PayloadMutate';
  contents: MutateType;
};

export type PayloadFind = {
  tag: 'PayloadFind';
  contents: Array<ASIndex>;
};

export type PayloadValue = {
  tag: 'PayloadValue';
  contents: [ASCompositeValue, ASLanguage];
};

export type PayloadCondFormatUpdate = {
  tag: 'PayloadCondFormatUpdate';
  contents: CondFormatRuleUpdate;
};

export type PayloadSetBarProp = {
  tag: 'PayloadSetBarProp';
  contents: [BarIndex, BarProp];
};

export type PayloadSheetUpdate = {
  tag: 'PayloadSheetUpdate';
  contents: SheetUpdate;
};

export type ASBackendPayload =
  PayloadN
  | PayloadInit
  | PayloadOpen
  | PayloadCL
  | PayloadLL
  | PayloadR
  | PayloadS
  | PayloadSelection
  | PayloadJump
  | PayloadSS
  | PayloadWB
  | PayloadWBS
  | PayloadWorkbookSheets
  | PayloadW
  | PayloadU
  | PayloadE
  | PayloadPaste
  | PayloadProp
  | PayloadXp
  | PayloadList
  | PayloadText
  | PayloadMutate
  | PayloadDrag
  | PayloadFind
  | PayloadCondFormatUpdate
  | PayloadSetBarProp
  | PayloadSheetUpdate;

export type ASBackendTime = {
  tag: 'Time';
  day: string;
  hour: number;
  minute: number;
  sec: number;
};

export type ASBackendWorkbookSheet = {
  tag: 'WorkbookSheet';
  wsName: string;
  wsSheets: Array<ASSheet>;
};

export type Success = {
  tag: 'Success';
};

export type Failure = {
  tag: 'Failure';
  failDesc: string;
};

export type NoResult = {
  tag: 'NoResult';
};

export type ASBackendResult = Success | Failure | NoResult;

export type ASMessageAction =
  'NoAction'
  | 'Acknowledge'
  | 'SetInitialSheet'
  | 'Find'
  | 'New' | 'Import' | 'Export' | 'ImportCSV'
  | 'Open' | 'Close'
  | 'Evaluate' | 'EvaluateRepl' | 'EvaluateHeader'
  | 'Decouple'
  | 'UpdateSheet'
  | 'Get' | 'Delete'
  | 'Copy' | 'Cut' | 'CopyForced'
  | 'Undo' | 'Redo'
  | 'Clear'
  | 'UpdateWindow'
  | 'SetBarProp'
  | 'SetProp' | 'ToggleProp'
  | 'Repeat'
  | 'BugReport'
  | 'JumpSelect'
  | 'MutateSheet'
  | 'Drag'
  | 'CondFormat' | 'UpdateCondFormatRules';

export type ASClientWindow = {
  window: NakedRange;
  sheetId: string;
};

export type ServerMessage = {
  action: ASMessageAction;
  payload: ASBackendPayload;
};

export type ASAPICallbackPair = {
  fulfill: (msg: ?ClientMessage) => void;
  reject: (msg: ?ClientMessage) => void;
};

export type ClientMessage = { 
  tag: "ClientMessage"; 
  clientAction: ClientAction; 
}

export type ClientAction = 
    NoAction
  | AskDecouple
  | SetInitialProperties
  | ShowFailureMessage
  | UpdateSheet 
  | ClearSheet
  | MakeSelection
  | LoadImportedCells
  | ShowHeaderResult;

export type NoAction = { 
  tag: "NoAction"; 
}

export type AskDecouple = { 
  tag: "AskDecouple"; 
}

export type SetInitialProperties = { 
  tag: "SetInitialProperties"; 
  contents: [SheetUpdate, Array<ASExpression>];
}

export type ShowFailureMessage = { 
  tag: "ShowFailureMessage"; 
  contents: string; 
}

export type UpdateSheet = { 
  tag: "UpdateSheet"; 
  contents: SheetUpdate; 
}

export type ClearSheet = { 
  tag: "ClearSheet"; 
  contents: string; 
}

export type MakeSelection = { 
  tag: "MakeSelection"; 
  contents: ASSelection; // ::ALEx:: need to sync with backend
}

export type LoadImportedCells = { 
  tag: "LoadImportedCells"; 
  contents: Array<ASCell>;
}

export type ShowHeaderResult = { 
  tag: "ShowHeaderResult"; 
  contents: ASCompositeValue;
}