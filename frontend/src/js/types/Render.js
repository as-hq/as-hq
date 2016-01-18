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

export type RenderParams = {
  mode: ?string,
  deps: Array<ASRange>,
  cellWidth: number,
  selection: ?ASSelection,
  selectionRect: ?PXRectangle,
  mouseoverError: number,
  dragRect: ?ASRange,
  shouldRenderSquareBox: boolean,
  boxWidth: number,
  topLeftBox: ?HGPoint,
  dragCorner: ?DragCorner,
  draggedBoxSelection: ?ASSelection
};
