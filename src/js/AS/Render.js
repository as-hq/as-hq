import RenderUtils from './RenderUtils';

let _renderParams = {
  mode: null, // null mode indicates normal behavior; any other string indicates otherwise
  deps: [],
  cellWidth: 100, // should be in Constants.js, but it makes things faster to put it here
  selection: null,
  selectionRect: null,
  mouseoverError: 5,
  dragRect: null,
  shouldRenderSquareBox: true,
  boxWidth: 6,
  topLeftBox: null, // {x,y} for the location of the top left corner of blue box, in pixels
  dragCorner: null, // {x,y} coordinate of corner of blue box dragging (not in pixels, in cells),
  draggedBoxSelection: null,
};

export default {

  /*************************************************************************************************************************/
  // Getter and setters for blue box

  getTopLeftBox() {
    return _renderParams.topLeftBox;
  },

  getBoxWidth() {
    return _renderParams.boxWidth;
  },

  setShouldRenderSquareBox(b) {
    _renderParams.shouldRenderSquareBox = b;
  },

  setDragCorner(locObj) {
    _renderParams.dragCorner = locObj;
  },

  getDragCorner() {
    return _renderParams.dragCorner;
  },

  getDottedSelection() {
    return _renderParams.draggedBoxSelection;
  },


  /*************************************************************************************************************************/
  // Other getters and setters

  setMode(mode) {
    _renderParams.mode = mode;
  },

  setSelection(sel) {
    _renderParams.selection = sel;
  },

  setDependencies(deps) {
    _renderParams.deps = deps;
  },

  setDragRect(rng) { _renderParams.dragRect = rng; },

  getDragRect() { return _renderParams.dragRect; },

  /*************************************************************************************************************************/
  // Misc utils

  withinSegment(p, endpoint, length) {
    return (endpoint - _renderParams.mouseoverError <= p) &&
           (endpoint + length + _renderParams.mouseoverError >= p);
  },

  // given mouse coordinates,
  // returns the edge ("top"/"left"/etc) hovered over by these
  isOnSelectionEdge(pX, pY) {
    let {x, y, width, height} = _renderParams.selectionRect;
    return (this.withinSegment(pX, x, width) && (this.withinSegment(pY, y, 0) ||
                                                 this.withinSegment(pY, y+height, 0)))
        || (this.withinSegment(pY, y, width) && (this.withinSegment(pX, x, 0) ||
                                                 this.withinSegment(pX, x+width, 0)));
  },

  
  /*************************************************************************************************************************/
  // Renderers

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
          if (typeof val === 'object') { // means that val must be an image
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
      this.config.minWidth = textWidth + (2 * colHEdgeOffset);
      // Hypergrid : this.config.minWidth = _renderParams.cellWidth;

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
        if (config.wrap && false) {
          // let h = RenderUtils.wrapText(gc, val, x + halignOffset, y + valignOffset, width, fontMetrics.height);
          // let {origin} = _renderParams.selection,
          //     grid = this.getGrid(),
          //     fixedRowCount = grid.getFixedRowCount(),
          //     scrollY = grid.getVScrollValue();
          //     originY = origin.row + fixedRowCount - scrollY - 1;
          // debugger;
          // this.getBehavior().setRowHeight(origin.row, h);
        } else {
        gc.fillText(val, x + halignOffset, y + valignOffset);
        }
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

    _renderParams.selectionRect = {x: x, y: y, width: width, height: height};
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

  dependencyRenderer: function(gc) {
    _renderParams.deps.forEach((dep) => {
      gc.beginPath();
      RenderUtils.drawRect(dep, this, gc);
      gc.lineWidth = 1;
      gc.strokeStyle = 'orange';
      gc.stroke();
    }, this);
  },

  draggingRenderer: function(gc) {
    if (_renderParams.dragRect !== null) {
      console.log("DRAWING DRAG:", JSON.stringify(_renderParams.dragRect));
      gc.beginPath();
      RenderUtils.drawRect(_renderParams.dragRect, this, gc);
      gc.lineWidth = 1;
      gc.strokeStyle = 'blue';
      gc.setLineDash([5,5]);
      gc.stroke();
    }
  },

  cornerBoxRenderer: function(gc) {
    if (!_renderParams.shouldRenderSquareBox) {
      return; // no box should show on double click
    } else {
      let grid = this.getGrid(),
          fixedColCount = grid.getFixedColumnCount(),
          fixedRowCount = grid.getFixedRowCount(),
          scrollX = grid.getHScrollValue(),
          scrollY = grid.getVScrollValue(),
          lastVisibleColumn = this.getVisibleColumns().slice(-1)[0],
          lastVisibleRow = this.getVisibleRows().slice(-1)[0];

      let {origin,range} = _renderParams.selection,
          tlX = Math.min(range.tl.col - 1, lastVisibleColumn) + fixedColCount,
          tlY = Math.min(range.tl.row - 1, lastVisibleRow) + fixedRowCount,
          brX = Math.min(range.br.col - 1, lastVisibleColumn) + fixedColCount,
          brY = Math.min(range.br.row - 1, lastVisibleRow) + fixedRowCount,
          tl = this._getBoundsOfCell(tlX-scrollX, tlY-scrollY),
          br = this._getBoundsOfCell(brX-scrollX, brY-scrollY),
          centerPointOfBox = {
            x:br.origin.x + br.extent.x,
            y:br.origin.y + br.extent.y
          },
          boxShouldBeVisible = range.br.col -1 < lastVisibleColumn && range.br.row -1 < lastVisibleRow;

      let boxWidth =_renderParams.boxWidth,
          topLeftBoxX = centerPointOfBox.x-boxWidth/2.0,
          topLeftBoxY = centerPointOfBox.y-boxWidth/2.0;

      // Update the render params for reading on mouse events
      // To easily check if the mouse is "in" the box
      _renderParams.topLeftBox = {
        x:topLeftBoxX,
        y:topLeftBoxY
      };

      // We also have a dotted rectangle, depending on horizontal/vertical position
      // There's a blue filled rectangle and a green dotted rectangle (which overlap, but seems OK)
      if (_renderParams.dragCorner !== null) {
        let {dragX,dragY} = _renderParams.dragCorner,
            // drag accounts for scrolling already
            drag = this._getBoundsOfCell(dragX, dragY),
            xInBounds = dragX >= tlX-scrollX && dragX <= brX-scrollX,
            yInBounds = dragY >= tlY-scrollY && dragY <= brY-scrollY,
            width = null, height = null, dottedTlY = null, dottedTlX = null, dottedRange = null;
        if (xInBounds) { // draw vertical dotted line
          width = br.origin.x + br.extent.x - tl.origin.x;
          dottedTlX = tl.origin.x;
          if (dragY >= brY-scrollY) {
            dottedTlY = br.origin.y + br.extent.y;
            height =  drag.origin.y + drag.extent.y - dottedTlY;
            RenderUtils.drawDottedVertical(gc,dottedTlX,dottedTlY,width,height);
            dottedRange = {
              tl: {col:tlX,row:tlY},
              br: {col:brX,row:dragY+scrollY}
            };
          } else if (dragY <= tlY-scrollY) {
            dottedTlY = tl.origin.y;
            height = drag.origin.y - dottedTlY;
            RenderUtils.drawDottedVertical(gc,dottedTlX,dottedTlY,width,height);
            dottedRange = {
              tl: {col:tlX,row:dragY+scrollY},
              br: {col:brX,row:brY}
            };
          }
        } else if (yInBounds) { // draw horizontal dotted line
          dottedTlY = tl.origin.y,
          height = br.origin.y + br.extent.y - tl.origin.y;
          if (dragX >= brX-scrollX) {
            dottedTlX = br.origin.x + br.extent.x;
            width = drag.origin.x + drag.extent.x - dottedTlX;
            RenderUtils.drawDottedHorizontal(gc,dottedTlX,dottedTlY,width,height);
            dottedRange = {
              tl: {col:tlX,row:tlY},
              br: {col:dragX+scrollX,row:brY}
            };
          } else if (dragX <= tlX-scrollX) {
            dottedTlX = tl.origin.x;
            width = drag.origin.x - dottedTlX;
            RenderUtils.drawDottedHorizontal(gc,dottedTlX,dottedTlY,width,height);
            dottedRange  = {
              tl: {col:dragX+scrollX,row:tlY},
              br: {col:brX,row:brY}
            };
          }
        }
        _renderParams.draggedBoxSelection = {origin:origin,range:dottedRange};
      }
      if (boxShouldBeVisible) {
        gc.beginPath();
        gc.rect(topLeftBoxX,topLeftBoxY,boxWidth,boxWidth);
        gc.fillStyle = '#0066ff';
        gc.fill();
      }
    }
  }

}
