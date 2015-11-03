let _renderParams = {
  mode: null, // null mode indicates normal behavior; any other string indicates otherwise
  deps: [],
  cellWidth: 100,
  selection: null
};

export default {
  defaultCellRenderer: {
    paint: function(gc, x, y, width, height, isLink) {
      isLink = isLink || false;
      var colHEdgeOffset = this.config.properties.cellPadding,
          halignOffset = 0,
          valignOffset = this.config.voffset,
          halign = this.config.halign,
          isColumnHovered = this.config.isColumnHovered,
          isRowHovered = this.config.isRowHovered,
          val = this.config.value;

      var leftIcon, rightIcon, centerIcon, ixoffset, iyoffset;

      if (val && val.constructor === Array) {
          leftIcon = val[0];
          rightIcon = val[2];
          val = val[1];
          if (typeof val === 'object') { // must be an image
              centerIcon = val;
              val = null;
          }
          if (leftIcon && leftIcon.nodeName !== 'IMG') {
              leftIcon = null;
          }
          if (rightIcon && rightIcon.nodeName !== 'IMG') {
              rightIcon = null;
          }
          if (centerIcon && centerIcon.nodeName !== 'IMG') {
              centerIcon = null;
          }
      }

      if (gc.font !== this.config.font) {
          gc.font = this.config.font;
      }
      if (gc.textAlign !== 'left') {
          gc.textAlign = 'left';
      }
      if (gc.textBaseline !== 'middle') {
          gc.textBaseline = 'middle';
      }

      var fontMetrics = this.config.getTextHeight(this.config.font);
      var textWidth = this.config.getTextWidth(gc, val);


      //we must set this in order to compute the minimum width
      //for column autosizing purposes
      // this.config.minWidth = textWidth + (2 * colHEdgeOffset);
      this.config.minWidth = _renderParams.cellWidth;

      if (halign === 'right') {
          //textWidth = this.config.getTextWidth(gc, this.config.value);
          halignOffset = width - colHEdgeOffset - textWidth;
      } else if (halign === 'center') {
          //textWidth = this.config.getTextWidth(gc, this.config.value);
          halignOffset = (width - textWidth) / 2;
      } else if (halign === 'left') {
          halignOffset = colHEdgeOffset;
      }

      halignOffset = Math.max(0, halignOffset);
      valignOffset = valignOffset + Math.ceil(height / 2);

      //fill background only if our bgColor is populated or we are a selected cell
      if (this.config.bgColor || this.config.isSelected) {
          gc.fillStyle = this.config.isSelected ? this.config.bgSelColor : this.config.bgColor;
          gc.fillRect(x, y, width, height);
      }

      //draw text
      var theColor = this.config.isSelected ? this.config.fgSelColor : this.config.fgColor;
      if (gc.fillStyle !== theColor) {
          gc.fillStyle = theColor;
          gc.strokeStyle = theColor;
      }
      if (val !== null) {
          gc.fillText(val, x + halignOffset, y + valignOffset);
      }

      if (isColumnHovered && isRowHovered) {
          gc.beginPath();
          if (isLink) {
              gc.beginPath();
              underline(this.config, gc, val, x + halignOffset, y + valignOffset + Math.floor(fontMetrics.height / 2), 1);
              gc.stroke();
              gc.closePath();
          }
      }

      var iconWidth = 0;
      if (leftIcon) {
          iyoffset = Math.round((height - leftIcon.height) / 2);
          ixoffset = Math.round((halignOffset - leftIcon.width) / 2);
          gc.drawImage(leftIcon, x + ixoffset, y + iyoffset);
          iconWidth = Math.max(leftIcon.width + 2);
      }
      if (rightIcon) {
          iyoffset = Math.round((height - rightIcon.height) / 2);
          ixoffset = Math.round((halignOffset - rightIcon.width) / 2);
          gc.drawImage(rightIcon, x + width - ixoffset - rightIcon.width, y + iyoffset);
          iconWidth = Math.max(rightIcon.width + 2);
      }
      if (centerIcon) {
          iyoffset = Math.round((height - centerIcon.height) / 2);
          ixoffset = Math.round((width - centerIcon.width) / 2);
          gc.drawImage(centerIcon, x + width - ixoffset - centerIcon.width, y + iyoffset);
          iconWidth = Math.max(centerIcon.width + 2);
      }
      this.config.minWidth = this.config.minWidth + 2 * (iconWidth);
    }
  },

  setMode(mode) {
    _renderParams.mode = mode;
  },

  setSelection(sel) {
    _renderParams.selection = sel;
  },

  selectionRenderer: function(gc) {
    var grid = this.getGrid();
    if (_renderParams.selection === null) return;

    var {origin, range} = _renderParams.selection;
    var selection = {origin: {x: range.tl.col - 1, y: range.tl.row - 1},
                 extent: {x: range.br.col - range.tl.col, y: range.br.row - range.tl.row }};
    var mouseDown = selection.origin;

    var visibleColumns = this.getVisibleColumns();
    var visibleRows = this.getVisibleRows();
    var fixedColCount = grid.getFixedColumnCount();
    var fixedRowCount = grid.getFixedRowCount();
    var lastVisibleColumn = visibleColumns[visibleColumns.length - 1];
    var lastVisibleRow = visibleRows[visibleRows.length - 1];
    var scrollX = grid.getHScrollValue();
    var scrollY = grid.getVScrollValue();

    var extent = selection.extent;

    var dpOX = Math.min(mouseDown.x, mouseDown.x + extent.x) + fixedColCount;
    var dpOY = Math.min(mouseDown.y, mouseDown.y + extent.y) + fixedRowCount;

    var originX = origin.col + fixedColCount - scrollX - 1;
    var originY = origin.row + fixedRowCount - scrollY - 1;
    var originCellBounds = this._getBoundsOfCell(originX, originY);

    //lets check if our selection rectangle is scrolled outside of the visible area
    if (dpOX > lastVisibleColumn) {
        return; //the top of our rectangle is below visible
    }
    if (dpOY > lastVisibleRow) {
        return; //the left of our rectangle is to the right of being visible
    }

    var dpEX = Math.max(mouseDown.x, mouseDown.x + extent.x);
    dpEX = Math.min(dpEX, 1 + lastVisibleColumn) + 2;

    var dpEY = Math.max(mouseDown.y, mouseDown.y + extent.y);
    dpEY = Math.min(dpEY, 1 + lastVisibleRow) + 2;

    var o = this._getBoundsOfCell(dpOX - scrollX, dpOY - scrollY).origin;
    var ox = Math.round((o.x === undefined) ? grid.getFixedColumnsWidth() : o.x);
    var oy = Math.round((o.y === undefined) ? grid.getFixedRowsHeight() : o.y);
    // var ow = o.width;
    // var oh = o.height;
    var e = this._getBoundsOfCell(dpEX - scrollX, dpEY - scrollY).origin;
    var ex = Math.round((e.x === undefined) ? grid.getFixedColumnsWidth() : e.x);
    var ey = Math.round((e.y === undefined) ? grid.getFixedRowsHeight() : e.y);
    // var ew = e.width;
    // var eh = e.height;
    var x = Math.min(ox, ex);
    var y = Math.min(oy, ey);
    var width = 1 + ex - ox;
    var height = 1 + ey - oy;
    if (x === ex) {
        width = ox - ex;
    }
    if (y === ey) {
        height = oy - ey;
    }
    if (width * height < 1) {
        //if we are only a skinny line, don't render anything
        return;
    }

    gc.rect(x, y, width, height);
    gc.lineWidth = 1;
    // gc.fillStyle = 'rgba(0, 0, 0, 0.2)';
    // gc.fill();
    if (_renderParams.mode === null) {
      gc.strokeStyle = 'blue';
    } else if (_renderParams.mode === 'cut' || _renderParams.mode === 'copy') {
      gc.strokeStyle = _renderParams.mode === 'cut' ? 'red' : 'blue';
      gc.stroke();
      gc.rect(x, y, width, height);
      gc.strokeStyle = 'white';
      gc.setLineDash(this.focusLineStep[Math.floor(10 * (Date.now() / 300 % 1)) % this.focusLineStep.length]);
    }
    gc.stroke();
    gc.closePath();
    gc.beginPath();
    gc.rect(originCellBounds.origin.x, originCellBounds.origin.y,
            originCellBounds.extent.x+1,
            originCellBounds.extent.y+1);
    gc.strokeStyle = 'blue';
    gc.lineWidth = 1;
    gc.stroke();
  },

  setDependencies(deps) {
    _renderParams.deps = deps;
  },

  dependencyRenderer: function(gc) {
    let grid = this.getGrid(),
        fixedColCount = grid.getFixedColumnCount(),
        fixedRowCount = grid.getFixedRowCount(),
        scrollX = grid.getHScrollValue(),
        scrollY = grid.getVScrollValue();

    _renderParams.deps.forEach((dep) => {
      let tlX = dep.tl.col + fixedColCount - 1,
          tlY = dep.tl.row + fixedRowCount - 1,
          brX = dep.br.col + fixedColCount - 1,
          brY = dep.br.row + fixedRowCount - 1,
          tl = this._getBoundsOfCell(tlX - scrollX, tlY - scrollY),
          br = this._getBoundsOfCell(brX - scrollX, brY - scrollY),
          oX = tl.origin.x,
          oY = tl.origin.y,
          eX = (br.origin.x - oX) + br.extent.x,
          eY = (br.origin.y - oY) + br.extent.y;

      gc.beginPath();
      gc.rect(oX, oY, eX, eY);
      gc.lineWidth = 1;
      gc.strokeStyle = 'orange';
      gc.stroke();
    }, this);
  }
}
