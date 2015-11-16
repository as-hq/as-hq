/* @flow */

import type {
  ASExcelExecError
} from './Errors';

import type {
  ASUserId,
  ASPermissions
} from './User';

export type NoValue = {
  tag: 'NoValue';
};

export type ValueNull = {
  tag: 'ValueNull';
};

export type ValueS = {
  tag: 'ValueS';
  contents: string;
};

export type ValueI = {
  tag: 'ValueI';
  contents: number;
};

export type ValueD = {
  tag: 'ValueD';
  contents: number;
};

export type ValueB = {
  tag: 'ValueB';
  contents: boolean;
};

export type ValueL = {
  tag: 'ValueL';
  contents: Array<ASValue>;
};

export type ValueImage = {
  tag: 'ValueImage';
  imagePath: string;
};

export type ValueObject = {
  tag: 'ValueObject';
  displayValue: string;
  objectType: string;
  jsonRepresentation: string;
};

export type ValueError = {
  tag: 'ValueError';
  errMsg: string;
  errType: string;
  file: string;
  position: number;
};

export type ValueExcelError = {
  tag: 'ValueExcelError';
  contents: ASExcelExecError;
};

export type RListKey = string;

export type RList = {
  tag: 'RList';
  contents: Array<[RListKey, ASValue]>;
};

export type RDataFrame = {
  tag: 'RDataFrame';
  contents: Array<ASValue>;
};

export type ColorTag = {
  tag: 'Color';
  contents: string;
};

export type SizeTag = {
  tag: 'Size';
  contents: number;
};

export type BoldTag = {
  tag: 'Bold';
};

export type ItalicTag = {
  tag: 'Italic';
};

export type UnderlineTag = {
  tag: 'Underline';
};

export type Bloomberg = {
  tag: 'Bloomberg';
  url: string;
  key: string;
};

export type StreamB = {
  tag: 'StreamB';
  contents: Bloomberg;
};

export type NoSource = {
  tag: 'NoSource';
};

export type StreamSource = StreamB | NoSource;

export type Stream = {
  tag: 'Stream';
  streamSource: StreamSource;
  streamFreq: number;
};

export type StreamTag = {
  tag: 'Stream';
  contents: Stream;
};

export type TrackingTag = {
  tag: 'Tracking';
};

export type VolatileTag = {
  tag: 'Volatile';
};

export type ReadOnlyTag = {
  tag: 'ReadOnly';
  contents: Array<ASUserId>;
};

export type ListMemberTag = {
  tag: 'ListMember';
  listKey: string;
};

export type NoFormat = {
  tag: 'NoFormat';
};

export type MoneyFormat = {
  tag: 'Money';
};

export type PercentageFormat = {
  tag: 'Percentage';
};

export type DateFormat = {
  tag: 'Date';
};

export type FormatType =
  NoFormat
  | MoneyFormat
  | PercentageFormat
  | DateFormat;

export type FormatTag = {
  tag: 'Format';
  contents: FormatType;
};

export type DFMemberTag = {
  tag: 'DFMember';
};

export type ImageDataTag = {
  tag: 'ImageData';
  imageWidth: number;
  imageHeight: number;
  imageOffsetX: number;
  imageOffsetY: number;
};

export type ASLanguage = string;

export type ASExpression = {
  expression: string;
  language: ?ASLanguage;
  dependencies?: Array<NakedRange>;
};

export type ASCellTag =
  ColorTag
  | SizeTag
  | BoldTag | ItalicTag | UnderlineTag
  | StreamTag
  | TrackingTag
  | VolatileTag
  | ReadOnlyTag
  | ListMemberTag
  | FormatTag
  | DFMemberTag
  | ImageDataTag;

export type ASValue =
  NoValue
  | ValueNull
  | ValueS
  | ValueI
  | ValueD
  | ValueB
  | ValueL
  | ValueImage
  | ValueObject
  | ValueError
  | ValueExcelError
  | RList
  | RDataFrame;

export type ASReplValue = {
  replValue: ASValue;
  replLang: ASLanguage;
};

export type NakedIndex = {
  row: number;
  col: number;
};

export type NakedRange = {
  tl: NakedIndex;
  br: NakedIndex;
};

export type ASIndex = {
  tag: 'index';
  sheetId: string;
  index: NakedIndex;
};

export type ASRange = {
  tag: 'range';
  sheetId: string;
  range: NakedRange;
};

export type ASLocation = ASIndex | ASRange;

export type ASSheet = {
  tag: 'Sheet';
  sheetId: string;
  sheetName: string;
  sheetPermissions: ASPermissions;
};

export type ASWorkbook = {
  tag: 'Workbook';
  workbookName: string;
  workbookSheets: Array<string>;
};

export type ASCell = {
  cellExpression: ASExpression;
  cellValue: ASValue;
  cellLocation: ASIndex;
  cellTags: Array<ASCellTag>;
};
