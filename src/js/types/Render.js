/* @flow */

import type {
  NakedRange,
  ASSelection
} from '../types/Eval';

export type Style = { [key: string]: (string|number) };

export type DragCorner = {
  dragX: number;
  dragY: number;
};

export type SelectionRect = {
  x: number;
  y: number;
  width: number;
  height: number;
};

export type RenderParams = {
  mode: ?string,
  deps: Array<NakedRange>,
  cellWidth: number,
  selection: ?ASSelection,
  selectionRect: ?SelectionRect,
  mouseoverError: number,
  dragRect: ?NakedRange,
  shouldRenderSquareBox: boolean,
  boxWidth: number,
  topLeftBox: ?HGPoint,
  dragCorner: ?DragCorner,
  draggedBoxSelection: ?ASSelection
};
