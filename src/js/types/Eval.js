/* @flow */

import type ASIndex from '../classes/ASIndex';
import type ASRange from '../classes/ASRange';

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
  contents: Array<any>; // should really be forced to make it [], don't know how to do that in flow. (Alex 12/17)
};

export type Italic = {
  tag: 'Italic';
  contents: Array<any>; // should really be forced to make it [], don't know how to do that in flow. (Alex 12/17)
};

export type Underline = {
  tag: 'Underline';
  contents: Array<any>; // should really be forced to make it [], don't know how to do that in flow. (Alex 12/17)
};

export type Volatile = {
  tag: 'Volatile';
  contents: Array<any>; // should really be forced to make it [], don't know how to do that in flow. (Alex 12/17)
};

export type Tracking = {
  tag: 'Tracking';
  contents: Array<any>; // should really be forced to make it [], don't know how to do that in flow. (Alex 12/17)
};

export type BooleanCellTag = 'Bold' | 'Italic' | 'Underline' | 'Volatile' | 'Tracking';

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

export type FormatType = 'NoFormat' | 'Money' | 'Percentage' | 'Date';

export type ASLanguage = 'Python' | 'R' | 'SQL' | 'Excel';

export type ASExpression = {
  expression: string;
  language: ?ASLanguage;
  dependencies?: Array<ASRange>; // #mustrefactor TODO
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

export type ASSelectionObject = {
  origin: NakedIndex;
  range: NakedRange;
};

export type ASIndexObject = {
  tag: 'index';
  sheetId: string;
  index: NakedIndex;
};

export type ASRangeObject = {
  tag: 'range';
  sheetId: string;
  range: NakedRange;
};

export type ASLocation = ASIndex | ASRange;

export type ASLocationObject = ASIndexObject | ASRangeObject;

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

export type ASCellObject = {
  cellExpression: ASExpression;
  cellValue: ASValue;
  cellLocation: ASIndexObject;
  cellProps: Array<ASCellProp>;
  expandingType?: ExpandingType;
  // The color of the rendered cell depends on the cell's expanding type (e.g., whether it's a list or dataframe or general object.)
  // For now, the easiest fast way to provide this information to the renderer is to just add this attribute to cells. An alternative
  // would be to store all the range descriptors in a store, and check whether each cell belongs in a range every time we render it, but
  // accessing a store is slow compared with accessing an object's member directly.
  cellRangeKey?: RangeKey;
  // Indicates whether the cell belongs to a range, and if so, the dimensions of the range. This should only ever be used
  // in conjunction with the RangeKeyStore to determine expandingType, and should only exist transiently -- namely, when an updated
  // cell is passed from the server, and before its expandingType is set (by looking up the cellRangeKey in the RangeDescriptorStore)
};

// backend actually has more fields than this, but this is all frontend needs
export type RangeDescriptor = {
  expandingType: ExpandingType;
  descriptorKey: RangeKey;
};

export type RangeKey = {
  keyIndex: ASIndexObject;
  keyDimensions: RangeKeyDimensions;
};

export type RangeKeyDimensions = {
  width: number;
  height: number;
};
