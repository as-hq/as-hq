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

export type ValueNaN = {
  tag: 'ValueNaN';
};

export type ValueInf = {
  tag: 'ValueInf';
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

export type ValueImage = {
  tag: 'ValueImage';
  imagePath: string;
};

export type ValueSerialized = {
  tag: 'ValueSerialized';
  serializedValue: string;
  displayName: string;
};

export type ValueError = {
  tag: 'ValueError';
  errorMsg: string;
  errorType: string;
};

export type TextColor = {
  tag: 'TextColor';
  contents: string;
};

export type FillColor = {
  tag: 'FillColor';
  contents: string;
};

export type VAlign = {
  tag: 'VAlign';
  contents: VAlignType;
};

export type HAlign = {
  tag: 'HAlign';
  contents: HAlignType;
};

export type FontSize = {
  tag: 'FontSize';
  contents: number;
};

export type FontName = {
  tag: 'FontName';
  contents: string;
};

export type ValueFormat = {
  tag: 'ValueFormat';
  formatType: FormatType;
};

export type ImageData = {
  tag: 'ImageData';
  imageWidth: number;
  imageHeight: number;
  imageOffsetX: number;
  imageOffsetY: number;
};

export type StreamInfo = {
  tag: 'StreamInfo';
  streamSource: StreamSource;
  streamFreq: number;
};

export type ReadOnly = {
  tag: 'ReadOnly';
  contents: Array<ASUserId>;
};

export type URL = {
  tag: 'URL';
  urlLink: string;
};

export type Bold = {
  tag: 'Bold';
};

export type Italic = {
  tag: 'Italic';
};

export type Underline = {
  tag: 'Underline';
};

export type Volatile = {
  tag: 'Volatile';
};

export type Tracking = {
  tag: 'Tracking';
};

export type Bloomberg = {
  tag: 'Bloomberg';
  url: string;
  key: string;
};

///////////////////////////////////////////////////////////////////////////////
// Alignment types

export type HAlignType = 'LeftAlign' | 'HCenterAlign' | 'RightAlign';
export type VAlignType = 'TopAlign' | 'VCenterAlign' | 'BottomAlign';

///////////////////////////////////////////////////////////////////////////////
// Streams

export type StreamB = {
  tag: 'StreamB';
  contents: Bloomberg;
};

export type NoSource = {
  tag: 'NoSource';
};

export type StreamSource = StreamB | NoSource;

///////////////////////////////////////////////////////////////////////////////
// Formats

export type FormatType = 'NoFormat' | 'MoneyFormat' | 'PercentageFormat' | 'DateFormat';

export type ASLanguage = 'Python' | 'R' | 'SQL' | 'Excel';

export type ASExpression = {
  expression: string;
  language: ?ASLanguage;
  expandingType?: ExpandingType;
  rangeKey?: string;
  dependencies?: Array<NakedRange>;
};

export type ExpandingType = 'List' | 'RList' | 'RDataFrame' | 'NPArray' | 'NPMatrix' | 'PDataFrame' | 'PSeries';

export type ASCellProp =
    TextColor
  | FillColor
  | VAlign
  | HAlign
  | FontSize
  | FontName
  | ValueFormat
  | ImageData
  | StreamInfo
  | ReadOnly
  | URL
  | Bold | Italic | Underline
  | Volatile
  | Tracking;

export type ASValue =
  NoValue
  | ValueNaN
  | ValueInf
  | ValueS
  | ValueI
  | ValueD
  | ValueB
  | ValueImage
  | ValueError
  | ValueSerialized;

export type ArrayCollection = {
  tag: 'A';
  contents: Array<ASValue>;
};

export type MatrixCollection = {
  tag: 'M';
  contents: Array<ArrayCollection>;
};

export type Collection = ArrayCollection | MatrixCollection;

export type RListKey = string;

export type VList = {
  tag: 'VList';
  contents: Collection;
};

export type VRList = {
  tag: 'VRList';
  contents: Array<[RListKey, ArrayCollection]>;
};

export type VRDataFrame = {
  tag: 'VRDataFrame';
  rdfLabels: ArrayCollection;
  rdfIndices: ArrayCollection;
  rdfValues: MatrixCollection;
};

export type VNPArray = {
  tag: 'VNPArray';
  contents: Collection;
};

export type VNPMatrix = {
  tag: 'VNPMatrix';
  contents: MatrixCollection;
};

export type VPDataFrame = {
  tag: 'VPDataFrame';
  dfLabels: ArrayCollection;
  dfIndices: ArrayCollection;
  dfData: MatrixCollection;
};

export type VPSeries = {
  tag: 'VPSeries';
  seriesIndices: ArrayCollection;
  seriesData: ArrayCollection;
};

export type ExpandingValue =
  VList
  | VRList
  | VRDataFrame
  | VNPArray
  | VNPMatrix
  | VPDataFrame
  | VPSeries;

export type Expanding = {
  tag: 'Expanding';
  contents: ExpandingValue;
};

export type CellValue = {
  tag: 'CellValue';
  contents: ASValue;
};

export type ASCompositeValue = Expanding | CellValue;

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

export type ASSelection = {
  origin: NakedIndex;
  range: NakedRange;
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
  cellProps: Array<ASCellProp>;
};
