/* @flow */

import type ASIndex from '../classes/ASIndex';

import type {
  Style
} from '../types/Render';

export type ASOverlaySpec = {
  id: string;
  renderElem: (style: Style) => ReactElement;
  imageWidth: number;
  imageHeight: number;
  dragOffsetLeft: number;
  dragOffsetTop: number;
  loc: ?ASIndex;
};

export type DragEventDetail = {
  position: {top: string, left: string};
};

export type ResizeEventDetail = {
  size: {width: number, height: number};
};
