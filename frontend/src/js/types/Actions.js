/* @flow */

import type ASCell from '../classes/ASCell';
import type ASCondFormatRule from '../classes/ASCondFormatRule';
import type ASIndex from '../classes/ASIndex';
import type ASRange from '../classes/ASRange';
import type ASSelection from '../classes/ASSelection';

import type {
  ASLocation,
  ASExpression,
  ASValue,
  ASCompositeValue,
  ASLanguage,
  RangeDescriptor,
  RangeKey,
  NakedIndex,
} from './Eval';

import type {
  ASOverlaySpec
} from './Hypergrid';

import type {
  ASViewingWindow
} from './State';

import type {
  ASBackendWorkbookSheet,
  ClientMessage,
  EvalHeader,
  MessageId
} from './Messages';

import type {
  Bar,
  BarIndex
} from './Bar';

import type {
  NotificationSpec
} from './Notifications';

export type AddOverlayAction = {
  _type: 'ADD_OVERLAY';
  overlay: ASOverlaySpec;
};

export type GotFailureAction = {
  _type: 'GOT_FAILURE';
  errorMsg: string;
};

export type ScrolledAction = {
  _type: 'SCROLLED';
  vWindow: ASViewingWindow;
};

export type GotSelectionAction = {
  _type: 'GOT_SELECTION';
  newSelection: ASSelection;
};

export type GotUpdatedCellsAction = {
  _type: 'GOT_UPDATED_CELLS';
  newCells: Array<ASCell>;
  oldLocs: Array<ASLocation>;
};

export type GotUpdatedRangeDescriptorsAction = {
  _type: 'GOT_UPDATED_RANGE_DESCRIPTORS';
  newRangeDescriptors: Array<RangeDescriptor>;
  oldRangeKeys: Array<RangeKey>;
};

export type GotUpdatedBarsAction = {
  _type: 'GOT_UPDATED_BARS';
  newBars: Array<Bar>;
  oldBarLocs: Array<BarIndex>;
};

export type ClearedAction = {
  _type: 'CLEARED';
};

export type ClearedSheetAction = {
  _type: 'CLEARED_SHEET';
  sheetId: string;
};

export type GotUpdatedWorkbooksAction = {
  _type: 'GOT_UPDATED_WORKBOOKS';
  workbooks: Array<ASBackendWorkbookSheet>
};

export type GotUpdatedRulesAction = {
  _type: 'GOT_UPDATED_RULES';
  newRules: Array<ASCondFormatRule>;
  oldRuleIds: Array<string>;
};

export type LanguageChangedAction = {
  _type: 'LANGUAGE_CHANGED';
  language: ASLanguage;
};

///////////////////////////////////////////////////////////////////////////////
// Header actions

export type HeaderEvaluatedAction = {
  _type: 'HEADER_EVALUATED';
  value: string;
  display: ?string;
};

export type HeaderDataResetAction = {
  _type: 'HEADER_DATA_RESET';
  headers: Array<EvalHeader>;
};

export type HeaderUpdatedAction = {
  _type: 'HEADER_UPDATED';
  expression: string;
  language: ASLanguage;
};

export type HeaderLanguageChangedAction = {
  _type: 'HEADER_LANGUAGE_CHANGED';
  language: ASLanguage;
};

///////////////////////////////////////////////////////////////////////////////
//

export type GotFindAction = {
  _type: 'GOT_FIND';
  findLocs: Array<ASIndex>;
};

export type GotNewWorkbooksAction = {
  _type: 'GOT_NEW_WORKBOOKS';
  workbooks: Array<ASBackendWorkbookSheet>;
};

export type DeletedWorkbooksAction = {
  _type: 'DELETED_WORKBOOKS';
  workbooks: Array<ASBackendWorkbookSheet>;
};

export type FindIncrementedAction = {
  _type: 'FIND_INCREMENTED';
};

export type FindDecrementedAction = {
  _type: 'FIND_DECREMENTED';
};

export type GridKeyPressedAction = {
  _type: 'GRID_KEY_PRESSED';
  xpStr: string;
  cursorPos: number;
};

export type EditorChangedAction = {
  _type: 'EDITOR_CHANGED';
  xpStr: string;
};

export type TextboxChangedAction = {
  _type: 'TEXTBOX_CHANGED';
  xpStr: string;
};

export type NormalSelChangedAction = {
  _type: 'NORMAL_SEL_CHANGED';
  expression: string;
  language: ASLanguage;
};

export type PartialRefChangeWithEditorAction = {
  _type: 'PARTIAL_REF_CHANGE_WITH_EDITOR';
  xpStr: string;
  excelStr: string; // TODO: can we impose any constraints here
};

export type PartialRefChangeWithGridAction = {
  _type: 'PARTIAL_REF_CHANGE_WITH_GRID';
  excelStr: string;
};

export type PartialRefChangeWithTextboxAction = {
  _type: 'PARTIAL_REF_CHANGE_WITH_TEXTBOX';
  xpStr: string;
  excelStr: string;
};

export type EscPressedAction = {
  _type: 'ESC_PRESSED';
};

export type BackendUpdatedAndCellsChangedAction = {
  _type: 'BACKEND_UPDATED_AND_CELLS_CHANGED';
};

export type EvalTriedToDecoupleAction = {
  _type: 'EVAL_TRIED_TO_DECOUPLE';
};

export type OpenCondFormattingDialogAction = {
  _type: 'OPEN_COND_FORMATTING_DIALOG';
};

export type CloseCondFormattingDialogAction = {
  _type: 'CLOSE_COND_FORMATTING_DIALOG';
};

export type OpenChartingDialogAction = {
  _type: 'OPEN_CHARTING_DIALOG';
};

export type CloseChartingDialogAction = {
  _type: 'CLOSE_CHARTING_DIALOG';
};

export type MarkSentAction = {
  _type: 'MARK_SENT';
  locations: Array<ASLocation>;
  messageId: MessageId;
};

export type MarkReceivedAction = {
  _type: 'MARK_RECEIVED';
  messageId: MessageId;
};

export type MarkAllReceivedAction = {
  _type: 'MARK_ALL_RECEIVED';
};

export type AddNotificationAction = {
  _type: 'ADD_NOTIFICATION';
  spec: NotificationSpec;
};

// This is for programmatic dismissal.
export type DismissNotificationAction = {
  _type: 'DISMISS_NOTIFICATION';
  uid: string;
};

// This updates the internal state of the store in response to the user
// dismissing a notification, and will not emit an event.
export type RemoveNotificationAction = {
  _type: 'REMOVE_NOTIFICATION';
  uid: string;
};

export type SetConnectingStateAction = {
  _type: 'SET_CONNECTING_STATE';
  isConnected: boolean;
};

export type WorkbookAction =
  GotUpdatedWorkbooksAction
  | GotNewWorkbooksAction
  | DeletedWorkbooksAction;

export type ASAction =
  AddOverlayAction
  | GotFailureAction
  | GotUpdatedWorkbooksAction
  | GotNewWorkbooksAction
  | DeletedWorkbooksAction
  | GotUpdatedRulesAction
  | ScrolledAction
  | HeaderDataResetAction
  | GotSelectionAction
  | GotUpdatedCellsAction
  | GotUpdatedRangeDescriptorsAction
  | GotUpdatedBarsAction
  | ClearedAction
  | ClearedSheetAction
  | LanguageChangedAction
  | HeaderEvaluatedAction
  | HeaderDataResetAction
  | HeaderUpdatedAction
  | HeaderLanguageChangedAction
  | GotFindAction
  | FindIncrementedAction
  | FindDecrementedAction
  // three-way data integration actions
  | GridKeyPressedAction
  | EditorChangedAction
  | TextboxChangedAction
  | NormalSelChangedAction
  | PartialRefChangeWithEditorAction
  | PartialRefChangeWithGridAction
  | PartialRefChangeWithTextboxAction
  | EscPressedAction
  | BackendUpdatedAndCellsChangedAction
  | EvalTriedToDecoupleAction
  | OpenCondFormattingDialogAction
  | CloseCondFormattingDialogAction
  | OpenChartingDialogAction
  | CloseChartingDialogAction
  | MarkSentAction
  | MarkReceivedAction
  | MarkAllReceivedAction
  | AddNotificationAction
  | DismissNotificationAction
  | RemoveNotificationAction
  | SetConnectingStateAction;
