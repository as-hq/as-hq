/* @flow */

import Util from './Util';

import type {
  DragCorner,
  RenderParams
} from '../types/Render';

import type {
  MessageMetadata
} from '../types/Messages';

import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

import CellStore from '../stores/ASCellStore';
import ProgressStore from '../stores/ASProgressStore';

import RenderU from './utils/Render';

let _renderParams : RenderParams = {
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
  inProgressTimeout: 75
};

// I suspect this file should get split up
const Renderers = {

  /*************************************************************************************************************************/
  // Getter and setters for blue box

  getTopLeftBox() : ?HGPoint {
    return _renderParams.topLeftBox;
  },

  getBoxWidth() : number {
    return _renderParams.boxWidth;
  },

  setShouldRenderSquareBox(b: boolean) {
    _renderParams.shouldRenderSquareBox = b;
  },

  setDragCorner(locObj: ?DragCorner) {
    _renderParams.dragCorner = locObj;
  },

  getDragCorner() : ?DragCorner {
    return _renderParams.dragCorner;
  },

  getDottedSelection() : ?ASSelection {
    return _renderParams.draggedBoxSelection;
  },


  /*************************************************************************************************************************/
  // Other getters and setters

  setMode(mode: ?string) {
    _renderParams.mode = mode;
  },

  setSelection(sel: ?ASSelection) {
    _renderParams.selection = sel;
  },

  setDependencies(deps: Array<ASRange>) {
    _renderParams.deps = deps;
  },

  setDragRect(rng: ?ASRange) { _renderParams.dragRect = rng; },

  getDragRect() : ?ASRange { return _renderParams.dragRect; },

  /*************************************************************************************************************************/
  // Misc utils

  withinSegment(p: number, endpoint: number, length: number) : boolean {
    return (endpoint - _renderParams.mouseoverError <= p) &&
           (endpoint + length + _renderParams.mouseoverError >= p);
  },

  // given mouse coordinates,
  // returns the edge ("top"/"left"/etc) hovered over by these
  isOnSelectionEdge(pX: number, pY: number) : boolean {
    if (_renderParams.selectionRect == null) {
      return false;
    }
    let {origin, extent} = _renderParams.selectionRect;
    return (Renderers.withinSegment(pX, origin.x, extent.x) && (Renderers.withinSegment(pY, origin.y, 0) ||
                                                 Renderers.withinSegment(pY, origin.y + extent.y, 0)))
        || (Renderers.withinSegment(pY, origin.y, extent.y) && (Renderers.withinSegment(pX, origin.x, 0) ||
                                                 Renderers.withinSegment(pX, origin.x + extent.x, 0)));
  },

  underline(config: HGRendererConfig, gc: GraphicsContext, text: string, x: number, y: number, thickness: number) {
    let width : number = config.getTextWidth(gc, text);

    switch (gc.textAlign) {
      case 'center':
          x -= (width / 2);
          break;
      case 'right':
          x -= width;
          break;
    }

    //gc.beginPath();
    gc.lineWidth = thickness;
    gc.moveTo(x + 0.5, y + 0.5);
    gc.lineTo(x + width + 0.5, y + 0.5);
  },

  /*************************************************************************************************************************/
  // Renderers

  defaultCellRenderer: ({
    paint(gc: GraphicsContext, x: number, y: number, width: number, height: number, isLink?: boolean) {
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
      var fillColor = this.config.isSelected ? this.config.fgSelColor : this.config.fgColor;
      if (gc.fillStyle !== fillColor) {
        gc.fillStyle = fillColor;
        gc.strokeStyle = fillColor;
      }

      if (val !== null) {
        if (this.config.wrap && false) {
          // let h = Util.Canvas.wrapText(gc, val, x + halignOffset, y + valignOffset, width, fontMetrics.height);
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
              this.underline(this.config, gc, val, x + halignOffset, y + valignOffset + Math.floor(fontMetrics.height / 2), 1);
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
  }: HGRendererObject),

  getCellRenderer(config: HGRendererConfig): HGRendererObject {
    let renderer = Renderers.defaultCellRenderer,
        col = config.x + 1,
        row = config.y + 1,
        cell = CellStore.getCell(ASIndex.fromNaked({col: col, row: row}));

    // tag-based cell styling
    if (cell != null) {
      Util.Render.valueToRenderConfig(config, cell.value);
      if (cell.expandingType != null) {
        Util.Render.expandingTypeToRenderConfig(config, cell.expandingType);
      }

      // If there are > 10 decimal places, truncate to first 10, unless the number of
      // decimal places to show is a cell property, in which case we truncate to that extent.
      Util.Render.formatNumberOfDecimalPlaces(config, cell.props);

      // props take highest precedence
      if (cell.props.length > 0) { // props take higher precedence
        Util.Render.propsToRenderConfig(config, cell.props);
      }
    } else {
      config.halign = 'center';
    }

    renderer.config = config;
    return renderer;
  },

  selectionRenderer(gc: GraphicsContext) {
    if (_renderParams.selection === null || _renderParams.selection === undefined) return;
    let {selection: {range, origin}} = _renderParams;

    let rect = Util.Canvas.drawRect(range, this, gc);
    _renderParams.selectionRect = rect;

    // optionally draw copy/cut ants
    gc.lineWidth = 1;
    if (_renderParams.mode === null) {
      gc.strokeStyle = 'blue';
    } else if ((_renderParams.mode === 'cut' || _renderParams.mode === 'copy') && !!rect) {
      gc.strokeStyle = _renderParams.mode === 'cut' ? 'red' : 'blue';
      gc.stroke();
      gc.rect(rect.origin.x, rect.origin.y, rect.extent.x, rect.extent.y);
      gc.strokeStyle = 'white';
      gc.setLineDash(this.focusLineStep[Math.floor(10 * (Date.now() / 300 % 1)) % this.focusLineStep.length]);
    }
    gc.stroke();
    gc.closePath();

    // // draw origin rectangle
    gc.beginPath();
    Util.Canvas.drawRect(origin.toRange(), this, gc);
    gc.strokeStyle = 'blue';
    gc.lineWidth = 1;
    gc.stroke();
  },

  dependencyRenderer(gc: GraphicsContext) {
    _renderParams.deps.forEach((dep) => {
      gc.beginPath();
      Util.Canvas.drawRect(dep, this, gc);
      gc.lineWidth = 1;
      gc.strokeStyle = 'orange';
      gc.stroke();
    }, this);
  },

  draggingRenderer(gc: GraphicsContext) {
    if (_renderParams.dragRect !== null && _renderParams.dragRect !== undefined) {
      gc.beginPath();
      if (_renderParams.dragRect === null || _renderParams.dragRect === undefined) {
        throw new Error('The drag rect was nullified by beginPath, apparently');
      }

      Util.Canvas.drawRect(_renderParams.dragRect, this, gc);
      gc.lineWidth = 1;
      gc.strokeStyle = 'blue';
      gc.setLineDash([5,5]);
      gc.stroke();
    }
  },

  cornerBoxRenderer(gc: GraphicsContext) {
    if (!_renderParams.shouldRenderSquareBox) {
      return; // no box should show on double click
    } else {
      const {
        fixedColCount,
        fixedRowCount,
        scrollX,
        scrollY,
        lastVisibleColumn,
        lastVisibleRow
      } = RenderU.getGridSpec(this);

      if (_renderParams.selection == null) {
        return;
      }
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
        if (_renderParams.dragCorner == null) {
          return;
        }
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
            Util.Canvas.drawDottedVertical(gc,dottedTlX,dottedTlY,width,height);
            dottedRange = ASRange.fromNaked({
              tl: {col:tlX,row:tlY},
              br: {col:brX,row:dragY+scrollY}
            });
          } else if (dragY <= tlY-scrollY) {
            dottedTlY = tl.origin.y;
            height = drag.origin.y - dottedTlY;
            Util.Canvas.drawDottedVertical(gc,dottedTlX,dottedTlY,width,height);
            dottedRange = ASRange.fromNaked({
              tl: {col:tlX,row:dragY+scrollY},
              br: {col:brX,row:brY}
            });
          }
        } else if (yInBounds) { // draw horizontal dotted line
          dottedTlY = tl.origin.y,
          height = br.origin.y + br.extent.y - tl.origin.y;
          if (dragX >= brX-scrollX) {
            dottedTlX = br.origin.x + br.extent.x;
            width = drag.origin.x + drag.extent.x - dottedTlX;
            Util.Canvas.drawDottedHorizontal(gc,dottedTlX,dottedTlY,width,height);
            dottedRange = ASRange.fromNaked({
              tl: {col:tlX,row:tlY},
              br: {col:dragX+scrollX,row:brY}
            });
          } else if (dragX <= tlX-scrollX) {
            dottedTlX = tl.origin.x;
            width = drag.origin.x - dottedTlX;
            Util.Canvas.drawDottedHorizontal(gc,dottedTlX,dottedTlY,width,height);
            dottedRange = ASRange.fromNaked({
              tl: {col:dragX+scrollX,row:tlY},
              br: {col:brX,row:brY}
            });
          }
        }
        if (dottedRange != null) {
          _renderParams.draggedBoxSelection = ASSelection.fromASLocations({
            origin: origin,
            range: dottedRange
          });
        }
      }
      if (boxShouldBeVisible) {
        gc.beginPath();
        gc.rect(topLeftBoxX,topLeftBoxY,boxWidth,boxWidth);
        gc.fillStyle = '#0066ff';
        gc.fill();
      }
    }
  },

  inProgressRenderer(gc: GraphicsContext) {
    const msgs = ProgressStore.getMessagesInProgress();
    const now = Date.now();
    for (const {messageTimestamp, locations} of msgs) {

      if (now - messageTimestamp > _renderParams.inProgressTimeout) {

        for (const loc of locations) {
          if (loc instanceof ASIndex) {
            Util.Canvas.showInProgress(loc.toNaked(), this, gc);
          } else {
            for (const idx of loc.toIndices()) {
              Util.Canvas.showInProgress(idx.toNaked(), this, gc);
            }
          }
        }

      }

    }
  },
};

export default Renderers;
