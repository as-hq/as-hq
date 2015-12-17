/* @flow */

import GeneralU from './utils/General';

import ArrayU from './utils/Array';
import BrowserU from './utils/Browser';
import CanvasU from './utils/Canvas';
import CellU from './utils/Cell';
import ClipboardU from './utils/Clipboard';
import ConversionU from './utils/Conversion';
import FileU from './utils/File';
import FormatU from './utils/Format';
import KeyU from './utils/Key';
import LocationU from './utils/Location';
import ParsingU from './utils/Parsing';
import RenderU from './utils/Render';
import ShortcutU from './utils/Shortcut';
import StringU from './utils/String';
import WorkbookU from './utils/Workbook';

export default {
  _: GeneralU,
  
  Array: ArrayU,
  Browser: BrowserU,
  Canvas: CanvasU,
  Cell: CellU,
  Clipboard: ClipboardU,
  Conversion: ConversionU,
  File: FileU,
  Format: FormatU,
  Key: KeyU,
  Location: LocationU,
  Parsing: ParsingU,
  Render: RenderU,
  Shortcut: ShortcutU,
  String: StringU,
  Workbook: WorkbookU
};
