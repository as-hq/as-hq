/* @flow */

import type {
  ASExecError,
  ASExcelExecError
} from './Errors';

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

export type BarType = 'ColumnType' | 'RowType';

export type BarProp = Dimension | FromCellProp;

export type Dimension = {
  tag: 'Dimension';
  contents: number;
};

export type FromCellProp = {
  tag: 'FromCellProp';
  contents: ASCellProp;
};

export type Bar = {
  tag: 'Bar';
  barIndex: BarIndex;
  barProps: Array<BarProp>;
};

export type BarIndex = {
  tag: 'BarIndex';
  barSheetId: string;
  barType: BarType;
  barNumber: number;
}

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

export type PayloadCommit = {
  tag: 'PayloadCommit';
  contents: ASBackendCommit;
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

export type PayloadCondFormat = {
  tag: 'PayloadCondFormat';
  condFormatRules: Array<CondFormatRule>;
};

export type PayloadSetBarProp = {
  tag: 'PayloadSetBarProp';
  contents: [BarIndex, BarProp];
};

export type PayloadSheetUpdate = {
  tag: 'PayloadSheetUpdate';
  contents: SheetUpdate;
};

export type CondFormatRule = {
  tag: 'CondFormatRule';
  condFormat: ASCellProp;
  condition: CondFormatCondition;
  cellLocs: Array<ASRange>;
};

export type CondFormatCondition =
  CustomCondition
  | GreaterThanCondition
  | LessThanCondition
  | GeqCondition
  | LeqCondition
  | EqualsCondition
  | NotEqualsCondition
  | IsEmptyCondition
  | IsNotEmptyCondition
  | IsBetweenCondition
  | IsNotBetweenCondition;

export type CustomCondition = {
  tag: 'CustomCondition';
  contents: ASExpression;
}
export type GreaterThanCondition = {
  tag: 'GreaterThanCondition';
  contents: ASExpression;
};

export type LessThanCondition = {
  tag: 'LessThanCondition';
  contents: ASExpression;
};

export type GeqCondition = {
  tag: 'GeqCondition';
  contents: ASExpression;
};

export type LeqCondition = {
  tag: 'LeqCondition';
  contents: ASExpression;
};

export type EqualsCondition = {
  tag: 'EqualsCondition';
  contents: ASExpression;
};

export type NotEqualsCondition = {
  tag: 'NotEqualsCondition';
  contents: ASExpression;
};

export type IsEmptyCondition = {
  tag: 'IsEmptyCondition';
};

export type IsNotEmptyCondition = {
  tag: 'IsNotEmptyCondition';
};

export type IsBetweenCondition = {
  tag: 'IsBetweenCondition';
  contents: [ASExpression, ASExpression];
};

export type IsNotBetweenCondition = {
  tag: 'IsNotBetweenCondition';
  contents: [ASExpression, ASExpression];
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
  | PayloadCommit
  | PayloadPaste
  | PayloadProp
  | PayloadXp
  | PayloadReplValue
  | PayloadList
  | PayloadText
  | PayloadMutate
  | PayloadDrag
  | PayloadFind
  | PayloadCondFormat
  | PayloadSetBarProp
  | PayloadSheetUpdate;

export type ASBackendTime = {
  tag: 'Time';
  day: string;
  hour: number;
  minute: number;
  sec: number;
};

export type ASBackendCommit = {
  tag: 'ASCommit';
  cellDiff: ASCellDiff;
  time: ASBackendTime;
};

export type SheetUpdate = {
  tag: 'SheetUpdate';
  cellUpdates: CellUpdate;
  barUpdates: BarUpdate;
  condFormatRulesUpdates: CondFormatRuleUpdate;
  //#incomplete updatedRangeDescriptors: DescriptorUpdate;
};

export type CellUpdate = {
  tag: 'Update';
  newVals: Array<ASCell>;
  oldKeys: Array<ASLocation>;
};

export type BarUpdate = {
  tag: 'Update';
  newVals: Array<Bar>;
  oldKeys: Array<BarIndex>;
};

// #incomplete currently dysfunctional
export type CondFormatRuleUpdate = {
  tag: 'Update';
  newVals: Array<CondFormatRule>;
  oldKeys: Array<any>;
};

export type ASCellDiff = {
  tag: 'CellDiff';
  beforeCells: Array<ASCell>;
  afterCells: Array<ASCell>;
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
  | 'New' | 'Import' | 'Export'
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
  | 'CondFormat' | 'SetCondFormatRules';

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
  action: 'SetCondFormatRules';
  payload: PayloadSheetUpdate;
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
  | EvaluateReplResponse
  | EvaluateHeaderResponse
  | FindResponse
  | SetCondFormatResponse;

export type ASAPICallbackPair = {
  fulfill: (msg: ?ASServerMessage) => void;
  reject: (msg: ?ASServerMessage) => void;
};
