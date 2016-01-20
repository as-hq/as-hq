/* @flow */

import type ASIndex from '../../classes/ASIndex';

import type {
  PXRectangle
} from '../../types/Render';

import ASRange from '../../classes/ASRange';
import RenderU from './Render';

// Not declaring HTMLImageElement for now.
let loadingImage: any = new Image();
loadingImage.src = "js/components/svg-loaders/svg-loaders/bars.svg";

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
    const {
      fixedColCount,
      fixedRowCount,
      scrollX,
      scrollY,
      firstVisibleColumn,
      firstVisibleRow,
      lastVisibleColumn,
      lastVisibleRow
    } = RenderU.getGridSpec(renderer);

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

  showInProgress(
    col: number,
    row: number,
    renderer: HGRendererElement,
    gc: GraphicsContext
  ) {
    const {
      fixedColCount,
      fixedRowCount,
      scrollX,
      scrollY,
      firstVisibleColumn,
      firstVisibleRow,
      lastVisibleColumn,
      lastVisibleRow
    } = RenderU.getGridSpec(renderer);

    const x = col - 1 + fixedColCount;
    const y = row - 1 + fixedRowCount;
    const shape = renderer._getBoundsOfCell(x - scrollX, y - scrollY);
    const oX = shape.origin.x;
    const oY = shape.origin.y;
    const eX = shape.extent.x;
    const eY = shape.extent.y;

    if (x < firstVisibleColumn || x > lastVisibleColumn ||
        y < firstVisibleRow || y > lastVisibleRow) {
        return;
    }

    gc.fillStyle = '#CFD8DC';
    gc.fillRect(oX, oY, eX, eY);
    gc.stroke();

    const progressWidth = 30;
    const progressHeight = 16;
    const progressOX = oX + (eX / 2) - (progressWidth / 2);
    const progressOY = oY + (eY / 2) - (progressHeight / 2);
    gc.drawImage(
      loadingImage,
      progressOX,
      progressOY,
      progressWidth,
      progressHeight
    );
    gc.stroke();
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
