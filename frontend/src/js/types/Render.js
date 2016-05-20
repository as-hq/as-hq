/* @flow */

import type ASRange from '../classes/ASRange';
import type ASSelection from '../classes/ASSelection';

export type Style = { [key: string]: (string|number) };

export type DragCorner = {
  dragX: number;
  dragY: number;
};

// PXRectangle has units in pixels.
export type PXRectangle = {
  origin: { x: number, y: number },
  extent: { x: number, y: number }
};

export type ClipboardMode =
  'disabled'
  | 'cut'
  | 'copy'
  ;

export type RenderParams = {
  clipboardMode: ClipboardMode,
  deps: Array<ASRange>,
  cellWidth: number,
  selection: ?ASSelection,
  selectionRect: ?PXRectangle,
  mouseoverError: number,
  dragRect: ?ASRange,
  boxWidth: number,
  topLeftBox: ?HGPoint,
  dragCorner: ?DragCorner,
  draggedBoxSelection: ?ASSelection,
  inProgressTimeout: number
};
