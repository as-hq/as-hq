/* @flow */

import type {
  PXRectangle
} from '../../types/Render';

import ASRange from '../../classes/ASRange';

export default {
  // The below two methods draw three sides of the rectangle
  drawDottedHorizontal(
    gc: GraphicsContext,
    ox: number,
    oy: number,
    ex: number,
    ey: number
  ) {
    gc.beginPath();
    gc.setLineDash([6]);
    gc.moveTo(ox,oy);
    gc.lineTo(ox+ex,oy);
    gc.lineTo(ox+ex,oy+ey);
    gc.lineTo(ox,oy+ey);
    gc.lineWidth = 1;
    gc.strokeStyle = 'black';
    gc.stroke();
  },

  drawDottedVertical(
    gc: GraphicsContext,
    ox: number,
    oy: number,
    ex: number,
    ey: number
  ) {
    gc.beginPath();
    gc.setLineDash([6]);
    gc.moveTo(ox,oy);
    gc.lineTo(ox,oy+ey);
    gc.lineTo(ox+ex,oy+ey);
    gc.lineTo(ox+ex,oy);
    gc.lineWidth = 1;
    gc.strokeStyle = 'black';
    gc.stroke();
  },

  drawRect (rng: ASRange, renderer: HGRendererElement, gc: GraphicsContext): ?PXRectangle {
    let grid = renderer.getGrid(),
        fixedColCount = grid.getFixedColumnCount(),
        fixedRowCount = grid.getFixedRowCount(),
        scrollX = grid.getHScrollValue(),
        scrollY = grid.getVScrollValue(),
        firstVisibleColumn = renderer.getVisibleColumns()[0],
        firstVisibleRow = renderer.getVisibleRows()[0],
        lastVisibleColumn = renderer.getVisibleColumns().slice(-1)[0],
        lastVisibleRow = renderer.getVisibleRows().slice(-1)[0];

    let tlX = Math.max(rng.tl.col - 1, firstVisibleColumn) + fixedColCount,
        tlY = Math.max(rng.tl.row - 1, firstVisibleRow) + fixedRowCount,
        brX = Math.min(rng.br.col - 1, lastVisibleColumn) + fixedColCount,
        brY = Math.min(rng.br.row - 1, lastVisibleRow) + fixedRowCount,
        tl = renderer._getBoundsOfCell(tlX - scrollX, tlY - scrollY),
        br = renderer._getBoundsOfCell(brX - scrollX, brY - scrollY),
        oX = tl.origin.x,
        oY = tl.origin.y,
        eX = (br.origin.x - oX) + br.extent.x,
        eY = (br.origin.y - oY) + br.extent.y;

    if (brX < firstVisibleColumn || tlX > lastVisibleColumn ||
        brY < firstVisibleRow || tlY > lastVisibleRow) {
        return;
    }
    gc.rect(oX, oY, eX, eY);
    return {origin: {x: oX, y: oY}, extent: {x: eX, y: eY}};
  },

  wrapText(
    gc: GraphicsContext,
    text: string,
    x: number,
    y: number,
    maxWidth: number,
    lineHeight: number
  ): number {
    let words = text.split(' ');
    let line = '';
    let height = 0;
    for(var n = 0; n < words.length; n++) {
      let testLine = line + words[n] + ' ';
      let testWidth = gc.measureText(testLine).width;
      if (testWidth > maxWidth && n > 0) {
        gc.fillText(line, x, y);
        line = words[n] + ' ';
        y += lineHeight;
        height += lineHeight;
      } else {
        line = testLine;
      }
    }
    gc.fillText(line, x, y);
    return height;
  }
};
