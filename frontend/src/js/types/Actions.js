/* @flow */

import type ASCell from '../classes/ASCell';
import type ASCondFormatRule from '../classes/ASCondFormatRule';
import type ASIndex from '../classes/ASIndex';
import type ASRange from '../classes/ASRange';
import type ASSelection from '../classes/ASSelection';

import type {
  ASLocation,
  ASIndexObject,
  ASExpression,
  ASValue,
  ASCompositeValue,
  ASLanguage,
  RangeDescriptor,
  RangeKey,
  NakedIndex,
  HAlignType,
  VAlignType,
  Offset
} from './Eval';

import type {Callback} from './Base';

import type {
  ASOverlaySpec
} from './Overlay';

import type {
  FocusedElement,
  BottomPaneType
} from './State';

import type {
  SheetUpdate
} from './Updates';

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
} from '../classes/Notification';

import type {
  ASSheet
} from './Eval';

export type ToggleShortcutHelper = {
  _type: 'TOGGLE_SHORTCUT_HELPER';
};

export type CloseShortcutHelper = {
  _type: 'CLOSE_SHORTCUT_HELPER';
};

export type AddOverlayWithoutLocAction = {
  _type: 'ADD_OVERLAY_WITHOUT_LOC';
  overlay: ASOverlaySpec;
};

export type OverlayResizedAction = {
  _type: 'OVERLAY_RESIZED';
  overlay: ASOverlaySpec;
  width: number;
  height: number;
};

export type OverlayDeletedAction = {
  _type: 'OVERLAY_DELETED';
  overlay: ASOverlaySpec;
};

export type GotFailureAction = {
  _type: 'GOT_FAILURE';
  errorMsg: string;
};

export type ScrolledAction = {
  _type: 'SCROLLED';
  vWindow: ASRange;
};

export type SetObjectViewAction = {
  _type: 'SET_OBJECT_VIEW';
  location: ASIndexObject;
  objectView: string;
};

export type GotSelectionAction = {
  _type: 'GOT_SELECTION';
  newSelection: ASSelection;
};

export type GotUpdatedRangeDescriptorsAction = {
  _type: 'GOT_UPDATED_RANGE_DESCRIPTORS';
  newRangeDescriptors: Array<RangeDescriptor>;
  oldRangeKeys: Array<RangeKey>;
};

export type ClearedAction = {
  _type: 'CLEARED';
};

export type ClearedSheetAction = {
  _type: 'CLEARED_SHEET';
  sheetId: string;
};

export type ChangedSheetAction = {
  _type: 'CHANGED_SHEET';
  sheetId: string;
};

export type GotMySheetsAction = {
  _type: 'GOT_MY_SHEETS';
  sheets: Array<ASSheet>;
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

export type OpenMenuItem = {
  _type: 'OPEN_TOOLBAR_ITEM';
  name: string;
};

export type CloseMenuItem = {
  _type: 'CLOSE_TOOLBAR_ITEM';
  name: string;
};

export type SetFormat = {
  _type: 'SET_FORMAT';
  format: string;
};

export type SetFont = {
  _type: 'SET_FONT';
  font: string;
};

export type SetHAlign = {
  _type: 'SET_HALIGN';
  alignment: HAlignType;
};

export type SetVAlign = {
  _type: 'SET_VALIGN';
  alignment: VAlignType;
};

export type LoginAttemptAction = {
  _type: 'LOGIN_ATTEMPT';
  token: string;
};

export type LoginSuccessAction = {
  _type: 'LOGIN_SUCCESS';
  userId: string;
  sheetId: string;
};

export type LoginCallbackRegisteredAction = {
  _type: 'LOGIN_CALLBACK_REGISTERED';
  cb: Callback;
};

export type WorkbookAction =
  GotUpdatedWorkbooksAction
  | GotNewWorkbooksAction
  | DeletedWorkbooksAction;

export type FocusedAction = {
  _type: 'FOCUSED';
  focus: FocusedElement;
};

export type ToggledFocusF2Action = {
  _type: 'TOGGLED_FOCUS_F2';
};

export type APIEvaluateAction = {
  _type: 'API_EVALUATE';
  offset: Offset;
};

export type StartEditingAction = {
  _type: 'START_EDITING';
  inputText: string;
  textboxHasFullFocus: boolean;
};

export type StopEditingAction = {
  _type: 'STOP_EDITING';
};

export type EditorSelectionChangedAction = {
  _type: 'EDITOR_SELECTION_CHANGED';
  selection: AESelection;
};

export type ExpressionChangedAction = {
  _type: 'EXPRESSION_CHANGED';
  expression: string;
};

export type ReferenceToggledAction = {
  _type: 'REFERENCE_TOGGLED';
};

export type HeaderToggledAction = {
  _type: 'HEADER_TOGGLED';
};

export type BottomPaneToggledAction = {
  _type: 'BOTTOM_PANE_TOGGLED';
  pane: BottomPaneType;
};

export type BottomPaneClosedAction = {
  _type: 'BOTTOM_PANE_CLOSED';
};

export type RepaintSpreadsheetAction = {
  _type: 'REPAINT_SPREADSHEET';
};

export type FindBarVisibilityChangedAction = {
  _type: 'FIND_BAR_VISIBILITY_CHANGED';
  isOpen: boolean;
};

export type FindModalVisibilityChangedAction = {
  _type: 'FIND_MODAL_VISIBILITY_CHANGED';
  isOpen: boolean;
};

export type FocusedTextboxFullyAction = {
  _type: 'FOCUSED_TEXTBOX_FULLY';
};

export type GridScrollOffsetAction = {
  _type: 'GRID_SCROLL_OFFSET';
  offset: Offset;
};

export type GridScrollDisabledAction = {
  _type: 'GRID_SCROLL_DISABLED';
};

export type HoveredAction = {
  _type: 'HOVERED';
  hover: FocusedElement;
};

export type SheetUpdatedAction = {
  _type: 'SHEET_UPDATED';
  update: SheetUpdate;
  sheetId: string;
};

export type ASAction =
  ToggleShortcutHelper
  | CloseShortcutHelper
  | AddOverlayWithoutLocAction
  | OverlayDeletedAction
  | OverlayResizedAction
  | GotFailureAction
  | GotUpdatedWorkbooksAction
  | GotNewWorkbooksAction
  | DeletedWorkbooksAction
  | GotUpdatedRulesAction
  | ScrolledAction
  | SetObjectViewAction
  | HeaderDataResetAction
  | GotSelectionAction
  | GotUpdatedRangeDescriptorsAction
  | ClearedAction
  | ClearedSheetAction
  | ChangedSheetAction
  | LanguageChangedAction
  | HeaderEvaluatedAction
  | HeaderDataResetAction
  | HeaderUpdatedAction
  | HeaderLanguageChangedAction
  | GotFindAction
  | FindIncrementedAction
  | FindDecrementedAction
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
  | SetConnectingStateAction
  | FocusedAction
  | ToggledFocusF2Action
  | SetConnectingStateAction
  | OpenMenuItem
  | CloseMenuItem
  | SetFormat
  | SetFont
  | SetHAlign
  | SetVAlign
  | LoginAttemptAction
  | LoginSuccessAction
  | LoginCallbackRegisteredAction
  | APIEvaluateAction
  | StartEditingAction
  | StopEditingAction
  | ExpressionChangedAction
  | EditorSelectionChangedAction
  | ReferenceToggledAction
  | HeaderToggledAction
  | BottomPaneToggledAction
  | BottomPaneClosedAction
  | RepaintSpreadsheetAction
  | FindBarVisibilityChangedAction
  | FindModalVisibilityChangedAction
  | FocusedTextboxFullyAction
  | HoveredAction
  | GridScrollOffsetAction
  | GridScrollDisabledAction
  | SheetUpdatedAction
  ;
