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
  ASLanguage,
  ASIndex,
  ASSheet,
  ASValue,
  ASCompositeValue,
  ASExpression,
  ASReplValue,
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

export type PayloadReplValue = {
  tag: 'PayloadReplValue';
  contents: ASReplValue;
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
  | PayloadReplValue
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

export type NoActionResponse = {
  action: 'NoAction';
  payload: PayloadN;
  result: ASBackendResult; // it only ever gets this with a failure.
};

export type NewResponse = {
  action: 'New';
  payload: PayloadWorkbookSheets | PayloadWB;
  result: ASBackendResult;
};

export type OpenResponse = {
  action: 'Open';
  payload: PayloadOpen;
  result: ASBackendResult;
};

export type UndoResponse = {
  action: 'Undo';
  payload: PayloadSheetUpdate;
  result: ASBackendResult;
};

export type RedoResponse = {
  action: 'Redo';
  payload: PayloadSheetUpdate;
  result: ASBackendResult;
};

export type UpdateResponse = {
  action: 'UpdateSheet';
  payload: PayloadSheetUpdate;
  result: ASBackendResult;
};

export type GetResponse = {
  action: 'Get';
  payload: PayloadSheetUpdate;
  result: ASBackendResult;
};

export type UpdateWindowResponse = {
  action: 'UpdateWindow';
  payload: PayloadSheetUpdate;
  result: ASBackendResult;
};

export type ClearResponse = {
  action: 'Clear';
  payload: PayloadS | PayloadN;
  result: ASBackendResult;
};

export type JumpSelectResponse = {
  action: 'JumpSelect';
  payload: PayloadSelection;
  result: ASBackendResult;
};

export type EvaluateReplResponse = {
  action: 'EvaluateRepl';
  payload: PayloadReplValue;
  result: ASBackendResult;
};

export type EvaluateHeaderResponse = {
  action: 'EvaluateHeader';
  payload: PayloadValue;
  result: ASBackendResult;
};

export type FindResponse = {
  action: 'Find';
  payload: PayloadFind;
  result: ASBackendResult;
};

export type SetCondFormatResponse = {
  action: 'UpdateCondFormatRules';
  payload: PayloadSheetUpdate;
  result: ASBackendResult;
};

export type ASClientWindow = {
  window: NakedRange;
  sheetId: string;
};

export type ServerMessage = {
  action: ASMessageAction;
  payload: ASBackendPayload;
};

export type ClientMessage =
  NoActionResponse
  | NewResponse
  | OpenResponse
  | UndoResponse
  | RedoResponse
  | UpdateResponse
  | GetResponse
  | UpdateWindowResponse
  | ClearResponse
  | JumpSelectResponse
  | EvaluateReplResponse
  | EvaluateHeaderResponse
  | FindResponse
  | SetCondFormatResponse;

export type ASAPICallbackPair = {
  fulfill: (msg: ?ClientMessage) => void;
  reject: (msg: ?ClientMessage) => void;
};
