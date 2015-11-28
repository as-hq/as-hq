/* @flow */

import type {
  ASExecError,
  ASExcelExecError
} from './Errors';

import type {
  NakedRange,
  ASRange,
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

export type ASBackendTime = {
  tag: 'Time';
  day: string;
  hour: number;
  minute: number;
  sec: number;
};

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

export type PayloadCommit = {
  tag: 'PayloadCommit';
  contents: ASBackendCommit;
};

export type PayloadDelete = {
  tag: 'PayloadDelete';
  contents: [ASRange, Array<ASCell>];
};

export type PayloadXp = {
  tag: 'PayloadXp';
  contents: ASExpression;
};

export type PayloadOpen = {
  tag: 'PayloadOpen';
  initHeaderExpressions: Array<ASExpression>;
  initCondFormatRules: Array<CondFormatRule>;
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
  contents: ASCompositeValue;
};

export type PayloadCondFormat = {
  tag: 'PayloadCondFormat';
  condFormatRules: Array<CondFormatRule>;
};

export type CondFormatRule = {
  tag: 'CondFormatRule';
  condFormat: ASCellProp;
  condition: ASExpression;
  cellLocs: Array<ASRange>;
};

export type ASBackendPayload =
  PayloadN
  | PayloadInit
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
  | PayloadCommit
  | PayloadDelete
  | PayloadPaste
  | PayloadProp
  | PayloadXp
  | PayloadOpen
  | PayloadReplValue
  | PayloadList
  | PayloadText
  | PayloadMutate
  | PayloadDrag
  | PayloadFind
  | PayloadCondFormat;

export type ASBackendCommit = {
  tag: 'ASCommit';
  before: Array<ASCell>;
  after: Array<ASCell>;
  time: ASBackendTime;
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
  | 'New' | 'Import'
  | 'Open' | 'Close'
  | 'Evaluate' | 'EvaluateRepl' | 'EvaluateHeader'
  | 'Update'
  | 'Get' | 'Delete'
  | 'Copy' | 'Cut' | 'CopyForced'
  | 'Undo' | 'Redo'
  | 'Clear'
  | 'UpdateWindow'
  | 'SetProp' | 'ToggleProp'
  | 'Repeat'
  | 'BugReport'
  | 'JumpSelect'
  | 'MutateSheet'
  | 'Drag';

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
  payload: PayloadCommit;
  result: ASBackendResult;
};

export type RedoResponse = {
  action: 'Redo';
  payload: PayloadCommit;
  result: ASBackendResult;
};

export type UpdateResponse = {
  action: 'Update';
  payload: PayloadN | PayloadCL | PayloadSS | PayloadWBS | PayloadWorkbookSheets;
  result: ASBackendResult;
};

export type GetResponse = {
  action: 'Get';
  payload: PayloadCL;
  result: ASBackendResult;
};

export type UpdateWindowResponse = {
  action: 'UpdateWindow';
  payload: PayloadCL;
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

export type DeleteResponse = {
  action: 'Delete';
  payload: PayloadDelete | PayloadWorkbookSheets;
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
};

export type FindResponse = {
  action: 'Find';
  payload: PayloadFind;
  result: ASBackendResult;
};

export type ASClientWindow = {
  window: NakedRange;
  sheetId: string;
};

export type ASClientMessage = {
  action: ASMessageAction;
  payload: ASBackendPayload;
};

export type ASServerMessage =
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
  | DeleteResponse
  | EvaluateReplResponse
  | EvaluateHeaderResponse
  | FindResponse;

export type ASAPICallbackPair = {
  fulfill: (msg: ?ASServerMessage) => void;
  reject: (msg: ?ASServerMessage) => void;
};
