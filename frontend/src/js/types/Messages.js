/* @flow */

import type ASRange from '../classes/ASRange';

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
  ASRangeObject,
  ASSelectionObject,
  ASLanguage,
  ASIndexObject,
  ASSheet,
  ASValue,
  HeaderResult,
  ASExpression,
  ASWorkbook,
  ASCellProp,
  ASCellObject,
  FormatType
} from './Eval';

import type {
  ASUserId
} from './User';

import type {
  CondFormatRule
} from './CondFormat';

export type MessageId = string;

export type MessageMetadata = {
  locations: Array<ASLocation>;
  messageTimestamp: number;
};

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
  contents: Array<ASCellObject>;
};

export type PayloadLL = {
  tag: 'PayloadLL';
  contents: Array<ASIndexObject>;
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
  selectionRange: ASRangeObject;
  selectionOrigin: ASIndexObject;
};

export type PayloadJump = {
  tag: 'PayloadJump';
  jumpRange: ASRangeObject;
  jumpOrigin: ASIndexObject;
  isShifted: boolean;
  jumpDirection: ASBackendDirection;
};

export type PayloadPaste = {
  tag: 'PayloadPaste';
  copyRange: ASRangeObject;
  copyTo: ASRangeObject;
};

export type PayloadProp = {
  tag: 'PayloadTag';
  prop: ASCellProp;
  tagRange: ASRangeObject;
};

export type PayloadText = {
  tag: 'PayloadText';
  text: String;
};

export type PayloadDrag = {
  tag: 'PayloadDrag';
  initialRange: ASRangeObject;
  dragRange: ASRangeObject;
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
  contents: ASRangeObject;
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
  contents: ASRange;
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
  contents: Array<ASIndexObject>;
};

// export type PayloadValue = {
//   tag: 'PayloadValue';
//   contents: [EvalResult, ASLanguage];
// };

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

export type ASClientWindow = {
  window: NakedRange;
  sheetId: string;
};

export type ServerMessage = {
  serverAction: ServerAction;
  messageId: string;
};

export type ServerAction =
    Initialize
  | InitializeDaemon
  | OpenSheet
  | RenameSheet
  | NewSheet
  | GetMySheets
  | UpdateWindow
  | Export
  | Evaluate
  | EvaluateHeader
  | SetLanguagesInRange
  | Get
  | GetBar
  | GetIsCoupled
  | Delete
  | ClearSheetServer
  | Undo
  | Redo
  | Copy
  | Cut
  | ToggleProp
  | SetProp
  | ChangeDecimalPrecision
  | Repeat
  | BugReport
  | MutateSheet
  | Drag
  | Decouple
  | Timeout
  | UpdateCondFormatRules
  | SetBarProp
  | ImportCSV
  | ImportExcel;

// These are the constructors of ServerAction.
export type ServerActionType =
    'Initialize'
  | 'InitializeDaemon'
  | 'OpenSheet'
  | 'RenameSheet'
  | 'NewSheet'
  | 'GetMySheets'
  | 'UpdateWindow'
  | 'Export'
  | 'Evaluate'
  | 'EvaluateHeader'
  | 'Get'
  | 'GetBar'
  | 'GetIsCoupled'
  | 'Delete'
  | 'ClearSheetServer'
  | 'Undo'
  | 'Redo'
  | 'Copy'
  | 'Cut'
  | 'ToggleProp'
  | 'SetProp'
  | 'ChangeDecimalPrecision'
  | 'Repeat'
  | 'BugReport'
  | 'MutateSheet'
  | 'Drag'
  | 'Decouple'
  | 'Timeout'
  | 'UpdateCondFormatRules'
  | 'SetBarProp'
  | 'ImportCSV'
  | 'ImportExcel';

export type Initialize = {
  tag: "Initialize";
  connUserId: string;
  connSheetId: string;
};

export type InitializeDaemon = {
  tag: "InitializeDaemon";
  parentUserId: string;
  parentLoc: ASIndexObject;
};

export type OpenSheet = {
  tag: "OpenSheet";
  contents: string;
};

export type RenameSheet = {
  tag: "RenameSheet";
  renameSheetId: string;
  newSheetName: string;
};

export type NewSheet = {
  tag: "NewSheet";
  contents: string;
};

export type GetMySheets = {
  tag: "GetMySheets";
  contents: Array<any>;
};

export type UpdateWindow = {
  tag: "UpdateWindow";
  contents: ASClientWindow;
};

export type Export = {
  tag: "Export";
  contents: string;
};

export type EvalInstruction = {
  tag: "EvalInstruction";
  evalXp:  ASExpression;
  evalLoc: ASIndexObject;
};

export type Evaluate = {
  tag: "Evaluate";
  contents: Array<EvalInstruction>;
};

export type EvalHeader = {
  tag: "EvalHeader";
  evalHeaderSheetId: string;
  evalHeaderLang: ASLanguage; 
  evalHeaderExpr: string; 
};

export type EvaluateHeader = {
  tag: "EvaluateHeader";
  contents: EvalHeader;
};

export type SetLanguagesInRange = {
  tag: "SetLanguagesInRange";
  contents: [ASLanguage, ASRangeObject];
};

export type Get = {
  tag: "Get";
  contents: Array<ASIndexObject>;
};

export type GetBar = {
  tag: "GetBar";
  contents: BarIndex;
};

export type GetIsCoupled = {
  tag: "GetIsCoupled";
  contents: ASIndexObject;
};

export type Delete = {
  tag: "Delete";
  contents: ASRangeObject;
};

export type ClearSheetServer = {
  tag: "ClearSheetServer";
  contents: string;
};

export type Undo = {
  tag: "Undo";
  contents: Array<any>; // really want a type for [], but don't know how to
};

export type Redo = {
  tag: "Redo";
  contents: Array<any>; // really want a type for [], but don't know how to
};

export type Copy = {
  tag: "Copy";
  copyFrom: ASRangeObject;
  copyTo: ASRangeObject;
};

export type Cut = {
  tag: "Cut";
  cutFrom: ASRangeObject;
  cutTo: ASRangeObject;
};

export type ToggleProp = {
  tag: "ToggleProp";
  contents: [ASCellProp, ASRangeObject];
};

export type SetProp = {
  tag: "SetProp";
  contents: [ASCellProp, ASRangeObject];
};

export type ChangeDecimalPrecision = {
  tag: "ChangeDecimalPrecision";
  contents: [number, ASRangeObject];
};

export type Repeat = {
  tag: "Repeat";
  contents: ASSelectionObject;
};

export type BugReport = {
  tag: "BugReport";
  contents: string;
};

export type MutateSheet = {
  tag: "MutateSheet";
  contents: MutateType;
};

export type Drag = {
  tag: "Drag";
  initialRange: ASRangeObject;
  dragRange: ASRangeObject;
};

export type Decouple = {
  tag: "Decouple";
  contents: Array<any>; // really want a type for [], but don't know how to
};

export type Timeout = {
  tag: "Timeout";
  contents: string; // the messageId of the message we want to kill
}

export type UpdateCondFormatRules = {
  tag: "UpdateCondFormatRules";
  newRules: Array<CondFormatRule>;
  oldRuleIds: Array<string>;
}

export type SetBarProp = {
  tag: "SetBarProp";
  contents: [BarIndex, BarProp];
}

export type ImportCSV = {
  tag: "ImportCSV";
  csvIndex: ASIndexObject;
  csvLang: ASLanguage;
  csvFileName: string;
}

export type ImportExcel = {
  tag: "ImportExcel";
  excelSheetId: string;
  excelFileName: string;
}

export type ASAPICallbackPair = {
  fulfill: (msg: ?ClientMessage) => void;
  reject: (msg: ?ClientMessage) => void;
}

export type ClientMessage = {
  tag: "ClientMessage";
  clientAction: ClientAction;
  messageId: string;
}

export type ClientAction =
    NoAction
  | AskDecouple
  | AskTimeout
  | SetSheetData
  | ShowFailureMessage
  | UpdateSheet
  | ClearSheet
  | SetMySheets
  | AskOpenSheet
  | MakeSelection
  | LoadImportedCells
  | HandleEvaluatedHeader;

export type NoAction = {
  tag: "NoAction";
  contents: Array<any>; // really want a type for [], but don't know how to
}

export type AskDecouple = {
  tag: "AskDecouple";
  contents: Array<any>; // really want a type for [], but don't know how to
}

export type AskTimeout = {
  tag: "AskTimeout";
  timeoutMessageId: string;
  serverActionType: ServerActionType;
}

export type SetSheetData = {
  tag: "SetSheetData";
  updateSheetId: string;
  update: SheetUpdate;
  headers: Array<EvalHeader>;
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

export type SetMySheets = {
  tag: "SetMySheets";
  mySheets: Array<ASSheet>;
  sharedSheets: Array<ASSheet>;
}

export type AskOpenSheet = {
  tag: "AskOpenSheet";
  contents: string;
}

export type MakeSelection = {
  tag: "MakeSelection";
  contents: ASSelectionObject;
}

export type LoadImportedCells = {
  tag: "LoadImportedCells";
  contents: Array<ASCellObject>;
}

export type HandleEvaluatedHeader = {
  tag: "HandleEvaluatedHeader";
  headerContents: EvalHeader;
  headerResult: HeaderResult; 
  headerEvaluator: string; 
}
