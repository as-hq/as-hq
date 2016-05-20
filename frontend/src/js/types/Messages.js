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
  ASValue,
  HeaderResult,
  ASExpression,
  ASCellProp,
  ASCellObject,
  FormatType,
  Sheet,
  Workbook,
  WorkbookRef,
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
  | CloneSheet
  | DeleteSheet
  | GetMySheets
  | UpdateWindow
  | Export
  | ExportCell
  | Evaluate
  | EvaluateHeader
  | GetObjectView
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
  | ImportExcel
  ;

// These are the constructors of ServerAction.
export type ServerActionType =
    'Initialize'
  | 'InitializeDaemon'
  | 'OpenSheet'
  | 'RenameSheet'
  | 'NewSheet'
  | 'CloneSheet'
  | 'DeleteSheet'
  | 'GetMySheets'
  | 'UpdateWindow'
  | 'Export'
  | 'ExportCell'
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
  | 'ImportExcel'
  ;

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

export type CloneSheet = {
  tag: "CloneSheet";
  contents: string;
};

export type DeleteSheet = {
  tag: "DeleteSheet";
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

export type ExportCell = {
  tag: 'ExportCell';
  contents: ASIndexObject;
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
  evalHeaderWorkbookId: string;
  evalHeaderLang: ASLanguage;
  evalHeaderExpr: string;
};

export type EvaluateHeader = {
  tag: "EvaluateHeader";
  contents: EvalHeader;
};

export type GetObjectView = {
  tag: "GetObjectView";
  contents: ASIndexObject;
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
  | ExportCellData
  | ShowFailureMessage
  | UpdateSheet
  | ClearSheet
  | SetOpenedWorkbook
  | SetMyWorkbooks
  | MakeSelection
  | LoadImportedCells
  | HandleEvaluatedHeader
  ;

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

export type ExportCellData = {
  tag: 'ExportCellData';
  exportedIndex: ASIndexObject;
  contents: string;
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

export type SetOpenedWorkbook = {
  tag: "SetOpenedWorkbook";
  contents: Workbook;
}

export type SetMyWorkbooks = {
  tag: "SetMyWorkbooks";
  contents: Array<WorkbookRef>;
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
