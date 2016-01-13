/* @flow */

import type {
  Callback
} from '../types/Base';

import type {InitCallback} from './types';

import Constants from '../Constants';
import Render from '../AS/Renderers';
import U from '../AS/Util';
import API from '../actions/ASApiActionCreators';
import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import ExpStore from '../stores/ASExpStore';
import SelectionStore from '../stores/ASSelectionStore';
import ASSpreadsheet from '../components/ASSpreadsheet.jsx';
import rowHeaderMenuItems from '../components/menus/RowHeaderMenuItems.jsx';
import columnHeaderMenuItems from '../components/menus/ColumnHeaderMenuItems.jsx';

import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

import _ from 'lodash';

import {convert} from './helpers';

const callbacks: Array<InitCallback> = [
  // add fin event callbacks
  ({ spreadsheet, hg }) => {
    let callbacks = ({
      /*
        Call onSelectionChange method in eval pane to deal with selection change
        Need to also figure out the expression to render in the editor
      */
      'fin-selection-changed': function (event) {
        ExpStore.setClickType(Constants.ClickType.CLICK);
        spreadsheet.props.onSelectionChange(spreadsheet.getSelectionArea());
      },

      'fin-scroll-x': function (event) {
        let scroll = spreadsheet.getScroll();
        if (event.detail.oldValue <= event.detail.value) {
          let newScrollPixels = {
            x: spreadsheet.state.scrollPixels.x + hg.getColumnWidth(event.detail.oldValue),
            y: spreadsheet.state.scrollPixels.y
          };
          spreadsheet.setState({scrollPixels: newScrollPixels, scroll: scroll});
        } else {
          let newScrollPixels = {
            x: spreadsheet.state.scrollPixels.x - hg.getColumnWidth(event.detail.value),
            y: spreadsheet.state.scrollPixels.y
          };
          spreadsheet.setState({scrollPixels: newScrollPixels, scroll: scroll});
        }
        if ((spreadsheet.getScroll()).x % 20 === 0) {
          ActionCreator.scroll(spreadsheet.getViewingWindow());
        }
      },

      'fin-scroll-y': function (event) {
        let scroll = spreadsheet.getScroll();
        if (event.detail.oldValue <= event.detail.value) {
          let newScrollPixels = {
            y: spreadsheet.state.scrollPixels.y + hg.getRowHeight(event.detail.oldValue),
            x: spreadsheet.state.scrollPixels.x
          };
          spreadsheet.setState({scrollPixels: newScrollPixels, scroll: scroll});
        } else {
          let newScrollPixels = {
            y: spreadsheet.state.scrollPixels.y - hg.getRowHeight(event.detail.value),
            x: spreadsheet.state.scrollPixels.x
          };
          spreadsheet.setState({scrollPixels: newScrollPixels, scroll: scroll});
        }
        if ((spreadsheet.getScroll()).y % 20 === 0) {
          ActionCreator.scroll(spreadsheet.getViewingWindow());
        }
      },

      'fin-double-click': function (event) {
        // should only fire when double click is inside grid. According to event.detail.gridCell here,
        // the top left grid cell is (0,0) which is different from e.g. the event in model.handleMouseDown.
        if (event.detail.gridCell.y >= 0 && event.detail.gridCell.x >= 0) {
          ExpStore.setClickType(Constants.ClickType.DOUBLE_CLICK);
          spreadsheet.refs.textbox.updateTextBox(ExpStore.getExpression());
          spreadsheet.props.setFocus('textbox');
        }
      }
    });

    _.forEach(callbacks, (v, k) => {
      hg.addFinEventListener(k, v);
    });
  },

  // set column and row widths
  ({ hg, spreadsheet, model }) => {
    // Calling setColumnWidth on hypergrid should make an API call to the backend to remember the changed
    // column width; we're overriding hypergrid's default setColumnWidth to do so. If we want to change the
    // column width without making an API call, use model._setColumnWidth
    hg.setColumnWidth = (columnIndex, columnWidth) => {
      spreadsheet.resizedColNum = columnIndex;
      model._setColumnWidth(columnIndex, columnWidth);
    }

    // Ditto
    hg.setRowHeight = (rowIndex, rowHeight) => {
      spreadsheet.resizedRowNum = rowIndex;
      model.setRowHeight(rowIndex, rowHeight);
    }
  },

  // handle cell clicking
  ({ spreadsheet, model }) => {
    // note: evt.gridCell in all these functions seem to think the coordinates of the top left cell is
    // (1,1) rather than (0,0).
    model.handleMouseDown = (grid, evt) => {
      if (evt.primitiveEvent.detail.primitiveEvent.shiftKey) { // shift+click
        let {origin} = spreadsheet.getSelectionArea(),
            newBr = {col: evt.gridCell.x, row: evt.gridCell.y};
        spreadsheet.select(
          ASSelection.fromASLocations({
            origin: origin,
            range: ASRange.fromASIndices({
              tl: origin,
              br: ASIndex.fromNaked(newBr)
            })
          }),
        false);
      } else {
        let {x, y} = spreadsheet.getCoordsFromMouseEvent(grid, evt);
        if (spreadsheet.insideBox(evt) && !evt.primitiveEvent.detail.isRightClick) {
          // dragging blue box
          spreadsheet.mouseDownInBox = true;
        } else if (Render.isOnSelectionEdge(x, y)) { // TODO: move spreadsheet the fuck out of Render, because
            /*  COMPLAINTS #mustrefactor

              - Render.isOnSelectionEdge is accessing global state modified by renderers
              - Render.isOnSelectionEdge should not be in AS/Renderers.js

                HERE ARE SOME SOLUTIONS (TODO)

              - Renderers.js should be moved into classes/RenderManager.js,
                with its renderParams renamed into the state of the RenderManager
              - Util/Render.js should be moved into RenderManager as well,
                as instance methods.
              - RenderManager is a singleton for the entire application, and
                initialized upon app initialization
            */
          // dragging selections
          spreadsheet.dragSelectionOrigin = ASIndex.fromGridCell(evt.gridCell);
        } else if (model.featureChain) {
          let clickedCell = evt.gridCell;
          // If the mouse is placed inside column header (not on a divider), we want to keep some extra state ourselves
          if (spreadsheet._clickedCellIsInColumnHeader(clickedCell) && spreadsheet._isLeftClick(evt)) {
           spreadsheet.clickedColNum = clickedCell.x;
          } else if (spreadsheet._clickedCellIsInRowHeader(clickedCell) && spreadsheet._isLeftClick(evt)) {
            spreadsheet.clickedRowNum = clickedCell.y;
          }
          model.featureChain.handleMouseDown(grid, evt);
          model.setCursor(grid);
        }
      }
    };
  },

  ({ spreadsheet, model }) => {
    model.onMouseMove = (grid, evt) => {
      let {x, y} = spreadsheet.getCoordsFromMouseEvent(grid, evt);
      if (spreadsheet.insideBox(evt)) {
        spreadsheet.setState({cursorStyle:'crosshair'});
      } else if (Render.isOnSelectionEdge(x, y)) {
        spreadsheet.setState({cursorStyle: 'move'});
      } else {
        if (spreadsheet.state.cursorStyle !== 'auto') {
          spreadsheet.setState({cursorStyle:'auto'});
        }
        if (model.featureChain) {
          model.featureChain.handleMouseMove(grid, evt);
          model.setCursor(grid);
        }
      }
    };
  },

  ({ spreadsheet, model }) => {
    model.onDoubleClick = (grid, evt) => {
      if (model.featureChain) {
        model.featureChain.handleDoubleClick(grid, evt);
        model.setCursor(grid);
      }

      // for now, double-clicking rows doesn't size it automatically

      if (spreadsheet._clickedCellIsInColumnHeader(evt.gridCell) && spreadsheet._isLeftClick(evt)) {
        // gridCell is 1-indexed, column indices in HG are 0-indexed, and we're using that
        // index as a reference in finishColumnResize().
        spreadsheet.resizedColNum = evt.gridCell.x - 1;
        spreadsheet.finishColumnResize();
      }
    };
  },

  ({ spreadsheet, model }) => {
    model.onMouseDrag = (grid, evt) => {
      let selOrigin = spreadsheet.dragSelectionOrigin;
      if (selOrigin != null) {
        // range dragging
        let {x, y} = spreadsheet.getCoordsFromMouseEvent(grid, evt);
        let {range} = spreadsheet.getSelectionArea();
        spreadsheet.drawDraggedSelection(selOrigin, range, evt.gridCell.x, evt.gridCell.y);
        spreadsheet.mousePosition = {x: evt.primitiveEvent.detail.mouse.x,
                              y: evt.primitiveEvent.detail.mouse.y};
        spreadsheet.scrollWithDraggables(grid);
        spreadsheet.repaint();
      } else if (spreadsheet.mouseDownInBox && !evt.primitiveEvent.detail.isRightClick) {
        // box dragging
        let {x,y} = evt.gridCell; // accounts for scrolling
        Render.setDragCorner({dragX: x, dragY: y});
        spreadsheet.mousePosition = {x: evt.primitiveEvent.detail.mouse.x,
                              y: evt.primitiveEvent.detail.mouse.y};
        spreadsheet.scrollWithDraggables(grid);
        spreadsheet.repaint(); // show dotted lines
      } else if (model.featureChain) {
        // If we've mouse down'ed on a column header, we're now dragging a column
        if (spreadsheet.clickedColNum !== null && spreadsheet._isLeftClick(evt)) {
          spreadsheet.draggingCol = true;
        } else if (spreadsheet.clickedRowNum !== null && spreadsheet._isLeftClick(evt)) {
          spreadsheet.draggingRow = true;
        }
        // do default
        model.featureChain.handleMouseDrag(grid, evt);
        model.setCursor(grid);
      }
    };
  },

  ({ spreadsheet, hg, model }) => {
    model.onMouseUp = (grid, evt) => { // handles both right click and mouse dragging
      let {which} = evt.primitiveEvent.detail.primitiveEvent;

      if (which === 3) { // right click
        /* x, y: against the page, for rendering the dropdown */
        /* relCol, relRow: cell coordinates, relative to top left of element, not A1 */
          /* for example if we are scrolled to A24 at top, A25 is 1 not 25 */
        let {
          gridCell: {x: relCol, y: relRow},
          primitiveEvent: {detail: {primitiveEvent: {offsetX: x, offsetY: y}}}
        } = evt;
        if (relCol !== 0 || relRow !== 0) { // right click on a row header
          let [col, row] =
            [relCol + hg.getHScrollValue(), relRow + hg.getVScrollValue()];
          spreadsheet.refs.rightClickMenu.openAt(x, y,
            (col != 0)
              ? columnHeaderMenuItems(col)
              : rowHeaderMenuItems(row)
          );
        }
      } else { // something is being dragged
        if (spreadsheet.dragSelectionOrigin !== null) { // draggable box case
          spreadsheet.dragSelectionOrigin = null;
          if (Render.getDragRect() === null) {
            return;
          }
          let {x, y} = spreadsheet.getCoordsFromMouseEvent(grid, evt);
          let sel = spreadsheet.getSelectionArea();
          let toRange = Render.getDragRect(),
              fromRange = sel.range;
          if (toRange != null) {
            spreadsheet.selectRange(toRange, false);
            Render.setDragRect(null);
            spreadsheet.repaint();
            API.cut(fromRange, toRange);
          }
        } else if (Render.getDragCorner() !== null) { // dragging one range to another
          let dottedSel = Render.getDottedSelection();
          Render.setDragCorner(null);
          spreadsheet.mouseDownInBox = false;
          // Do nothing if the mouseup isn't in the right column or row
          if (dottedSel != null && dottedSel.range !== null) {
            let activeSelection = SelectionStore.getActiveSelection();
            if (!! activeSelection) {
              API.drag(activeSelection.range, dottedSel.range);
              spreadsheet.select(dottedSel,true);
            }
          }
        } else if (model.featureChain) { // resizing a bar or dragging a bar to another location
          model.featureChain.handleMouseUp(grid, evt);
          model.setCursor(grid);

          Render.setDragCorner(null);
          spreadsheet.mouseDownInBox = false;

          // Clean up dragging a column, and send an API message to backend to swap data
          if (spreadsheet.draggingCol) {
            let destColNum = Math.max(1, evt.gridCell.x); // evt.gridCell.x can go negative...
            if (spreadsheet.clickedColNum != null && spreadsheet.clickedColNum != destColNum) {
              API.dragCol(spreadsheet.clickedColNum, destColNum);
            }
          } else if (spreadsheet.draggingRow) {
            let destRowNum = Math.max(1, evt.gridCell.y);
            if (spreadsheet.clickedRowNum != destRowNum && spreadsheet.clickedRowNum != null) {
              API.dragRow(spreadsheet.clickedRowNum, destRowNum);
            }
          }

          spreadsheet.clickedColNum = null;
          spreadsheet.draggingCol = false;
          spreadsheet.clickedRowNum = null;
          spreadsheet.draggingRow = false;

          // Ditto for resizing
          spreadsheet.finishColumnResize();
          spreadsheet.finishRowResize();
        }
      }
    };
  }
];

export default convert(callbacks);
