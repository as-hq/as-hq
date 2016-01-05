/* @flow */

import type {
  NakedRange,
  ASSelectionObject
} from '../types/Eval';

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
  deps: Array<NakedRange>,
  cellWidth: number,
  selection: ?ASSelectionObject,
  selectionRect: ?PXRectangle,
  mouseoverError: number,
  dragRect: ?NakedRange,
  shouldRenderSquareBox: boolean,
  boxWidth: number,
  topLeftBox: ?HGPoint,
  dragCorner: ?DragCorner,
  draggedBoxSelection: ?ASSelectionObject
};
