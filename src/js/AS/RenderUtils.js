export default {
  // The below two methods draw three sides of the rectangle
  drawDottedHorizontal: (gc,ox,oy,ex,ey) => {
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

  drawDottedVertical: (gc,ox,oy,ex,ey) => {
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

  drawRect: (rng, renderer, gc) => {
    let grid = renderer.getGrid(),
        fixedColCount = grid.getFixedColumnCount(),
        fixedRowCount = grid.getFixedRowCount(),
        scrollX = grid.getHScrollValue(),
        scrollY = grid.getVScrollValue(),
        firstVisibleColumn = renderer.getVisibleColumns()[0],
        firstVisibleRow = renderer.getVisibleRows()[0], 
        lastVisibleColumn = renderer.getVisibleColumns().slice(-1)[0],
        lastVisibleRow = renderer.getVisibleRows().slice(-1)[0];

    let tlX = rng.tl.col - 1 + fixedColCount,
        tlY = rng.tl.row - 1 + fixedRowCount,
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
  },

  wrapText(gc, text, x, y, maxWidth, lineHeight) {
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



}
