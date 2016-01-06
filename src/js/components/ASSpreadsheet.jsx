/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASRangeObject,
  ASCellObject,
  ASIndexObject,
  ASSelectionObject
} from '../types/Eval';

import type {
  ASOverlaySpec
} from '../types/Hypergrid';

import type {
  ASCursorStyle,
  ASViewingWindow,
  ASFocusType
} from '../types/State';

import {logDebug, logError} from '../AS/Logger';
import {catMaybes} from '../AS/Maybe';

import _ from 'lodash';

import React from 'react';
let ReactDOM = require('react-dom');

import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import ExpActionCreator from '../actions/ASExpActionCreators';

import API from '../actions/ASApiActionCreators';

import CellStore from '../stores/ASCellStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import SelectionStore from '../stores/ASSelectionStore';
import FindStore from '../stores/ASFindStore';
import ExpStore from '../stores/ASExpStore';
import BarStore from '../stores/ASBarStore.js';
import OverlayStore from '../stores/ASOverlayStore';

import U from '../AS/Util';
let {
  Conversion: TC,
  Key: KeyUtils,
  Shortcut: ShortcutUtils
} = U;

import Constants from '../Constants';
import Render from '../AS/Renderers';

import ASRightClickMenu from './basic-controls/ASRightClickMenu.jsx';
import ASOverlay from './ASOverlay.jsx';
import Textbox from './Textbox.jsx'

// $FlowFixMe: this module clearly exists, but flow can't find it??!
import Dropzone from 'react-dropzone';

import hgPatches from '../hg-patches/index';

let finRect: HGRectangleElement = (document.createElement('fin-rectangle'): any);

type ASSpreadsheetDefaultProps = {
  behavior: string;
  onReady: () => void;
};

type ASSpreadsheetProps = {
  hideToast: () => void;
  setFocus: (elem: ASFocusType) => void;
  onReady: () => void;
  onTextBoxDeferredKey: (e: SyntheticKeyboardEvent) => void;
  onFileDrop: (files: Array<File>) => void;
  onSelectionChange: (sel: ASSelectionObject) => void;
  onNavKeyDown: (e: SyntheticKeyboardEvent) => void;
  behavior: string;
  width?: string;
  height?: string;
};

type ASSpreadsheetState = {
  scroll: HGPoint;
  scrollPixels: HGPoint;
  overlays: Array<ASOverlaySpec>;
  cursorStyle: ASCursorStyle;
  selectionDraggable: boolean;
};

// REPL stuff is getting temporarily phased out in favor of an Eval Header file. (Alex 11/12)
export default class ASSpreadsheet
  extends React.Component<ASSpreadsheetDefaultProps, ASSpreadsheetProps, ASSpreadsheetState>
{
  /*************************************************************************************************************************/
  // Non-rendering state

  mousePosition: ?HGPoint;
  mouseDownInBox: boolean;
  dragSelectionOrigin: ?NakedIndex;

  draggingCol: boolean;
  draggingRow: boolean;
  clickedColNum: ?number;
  clickedRowNum: ?number;
  resizedColNum: ?number;
  resizedRowNum: ?number;

  gridProperties: {
    editorActivationKeys: Array<any>,
    scrollbarHoverOff: string,
    columnAutosizing: boolean
  };

  /*************************************************************************************************************************/
  // React methods

  constructor(props: ASSpreadsheetDefaultProps) {
    super(props);

    this.mousePosition = null;
    this.mouseDownInBox = false;
    this.dragSelectionOrigin = null;

    this.draggingCol = false;
    this.draggingRow = false;
    this.clickedColNum = null;
    this.clickedRowNum = null;
    this.resizedColNum = null;
    this.resizedRowNum = null;

    this.gridProperties = {
      editorActivationKeys: [],
      scrollbarHoverOff: 'visible',
      columnAutosizing: false,
      defaultFixedColumnWidth: 35,
      defaultColumnWidth: 100
    }

    this.state = {
      // keep scroll values in state so overlays autoscroll with grid
      scroll: { x: 0, y: 0 },
      scrollPixels: { x: 0, y: 0},
      overlays: [],
      cursorStyle: 'auto',
      selectionDraggable: false
    };
  }

  componentDidMount() {
    // Be able to respond to events from ExpStore
    ExpStore.addChangeListener(this._onExpressionChange.bind(this));
    BarStore.addChangeListener(this._onBarPropsChange.bind(this));
    OverlayStore.addChangeListener(this._onOverlaysChange.bind(this));

    // Hypergrid initialization
    document.addEventListener('polymer-ready', () => {
      this.props.onReady();
      this.initHypergrid();
      this.getInitialData();
    });
  }

  initHypergrid() {
    hgPatches.forEach((patch) => { patch(this); });

    let ind = {row: 1, col: 1};

    // This will make the first selection have properties of a click
    // Namely, the blue box will show up
    ExpStore.setClickType(Constants.ClickType.CLICK);
    this.select({origin: ind, range: {tl: ind, br: ind}}, false);
  }

  componentWillUnmount() {
    ExpStore.removeChangeListener(this._onExpressionChange.bind(this));
  }

  /*************************************************************************************************************************/
  // Handle mouse events by overriding hypergrid default

  getCoordsFromMouseEvent(grid: HGElement, evt: HGMouseEvent): HGPoint {
    let {x, y} = evt.mousePoint,
        point = finRect.point.create(evt.gridCell.x, evt.gridCell.y),
        {origin} = grid.getBoundsOfCell(point),
        pX = origin.x + x,
        pY = origin.y + y;
    return {x: pX, y: pY};
  }

  drawDraggedSelection(dragOrigin: NakedIndex, selRange: NakedRange, targetX: number, targetY: number) {
    let dX = targetX - dragOrigin.col,
        dY = targetY - dragOrigin.row;
    let range = U.Location.offsetRange(selRange, dY, dX);
    Render.setDragRect(range);
  }

  // Is the mouse location inside a blue box
  insideBox(event: HGMouseEvent): boolean {
    let {x, y} = event.primitiveEvent.detail.mouse,
       topLeftBox = Render.getTopLeftBox(),
       boxWidth   = Render.getBoxWidth();
    return U.Location.mouseLocIsContainedInBox(x,y,topLeftBox,boxWidth);
  }

  // Semi-recursive function via timeouts -- this is how hypergrid does it
  // Need to scroll even if no mouse event, but you're at the edge of the grid
  scrollWithDraggables(grid: HGElement) {
    if (this.mouseDownInBox || this.dragSelectionOrigin !== null) {

      let mousePos = this.mousePosition;
      if (! mousePos) {
        logDebug('No mouse position');
        return;
      }

      let {x, y} = mousePos,
          b = grid.getDataBounds(),
          numFixedColumns = grid.getFixedColumnCount(),
          numFixedRows = grid.getFixedRowCount(),
          dragEndInFixedAreaX = x < numFixedColumns,
          dragEndInFixedAreaY = y < numFixedRows;
      let xOffset = 0,
          yOffset = 0;
      if (x > b.origin.x + b.extent.x) {
        xOffset = 1;
      } else if (x < b.origin.x) {
        xOffset = -1;
      }
      if (y > b.origin.y + b.extent.y) {
        yOffset = 1;
      } else if (y < b.origin.y) {
        yOffset = -1;
      }
      let dragCellOffsetX = dragEndInFixedAreaX ? 0 : xOffset,
          dragCellOffsetY = dragEndInFixedAreaY ? 0 : yOffset;
      if (xOffset !== 0 || yOffset !== 0) {
        grid.scrollBy(xOffset, yOffset);
        grid.repaint();
      }
      // The below number affects scrolling rate, not sure what it should be
      setTimeout(this.scrollWithDraggables.bind(this, grid), 5000);
    }
  }

  /*************************************************************************************************************************/
  // Default getter methods, relating to location/scrolling/selection

  _getHypergrid(): HGElement {
    return React.findDOMNode(this.refs.hypergrid);
  }

  _getBehavior(): HGBehaviorElement {
    return this._getHypergrid().getBehavior();
  }

  getSelectionArea(): ASSelectionObject {
    let hg = this._getHypergrid(),
        selection = hg.getSelectionModel().getSelections()[0],
        ul = selection.origin,
        range = U.Location.orientRange({
                  tl: {row:  ul.y + 1,
                       col:  ul.x + 1},
                  br: {row: ul.y + selection.height() + 1,
                       col: ul.x + selection.width() + 1}
                }),
        sel = {
          range: range,
          origin: {row: ul.y + 1, col: ul.x + 1}
        };
    return sel;
  }

  getScroll(): HGPoint {
    let hg = this._getHypergrid();
    return {x: hg.hScrollValue, y: hg.vScrollValue};
  }

  getViewingWindow(): ASViewingWindow {
    let hg = this._getHypergrid(),
        [vs, hs] = [hg.vScrollValue, hg.hScrollValue],
        [cols, rows] = [hg.getVisibleColumns(), hg.getVisibleRows()];
        let colLength = cols.length, rowLength = rows.length;
        // This might fail on the initial load, since getVisibleColumns() and
        // getVisibleRows() might return nothing, ergo the below hack.
        if (colLength == 0) colLength = 20;
        if (rowLength == 0) rowLength = 30;
    return { range: {tl: {row: vs+1, col: hs+1},
                     br: {row: vs + rowLength - 1, col: hs + colLength - 1}} };
    // getVisibleColumns and getVisibleRows were manually modified to show one more
    // column/row than what hypergrid says is visible (...since they're actually visible)
    // but that messed with the boundaries shown here, which is why we're subtracting 1
    // from rowLength and colLength.
  }

  isVisible(col: number, row: number): boolean { // faster than accessing hypergrid properties
    return (this.state.scroll.x <= col && col <= this.state.scroll.x+Constants.numVisibleCols) &&
           (this.state.scroll.y <= row && row <= this.state.scroll.y+Constants.numVisibleRows);
  }

  getVisibleRows(): number {

    return this._getHypergrid().getVisibleRows().length;
  }

  getTextboxPosition(): ?HGRectangle {
    let scroll = this.state.scroll;
    let activeSelection = SelectionStore.getActiveSelection();
    if (activeSelection) {
      let {col, row} = activeSelection.origin,
          point = finRect.point.create(col - scroll.x, row - scroll.y);
      return this._getHypergrid().getBoundsOfCell(point);
    } else {
      return null;
    }
  }


  /*************************************************************************************************************************/
  // Hypergrid initialization

  finishColumnResize() {
    let col = this.resizedColNum;
    if (col != null) {
      let hg = this._getHypergrid(),
          width = hg.getColumnWidth(col);
      API.setColumnWidth(col+1, width);
      // column index on DB is 1-indexed, while for hypergrid it's 0-indexed.
      this.resizedColNum = null;
    }
  }

  finishRowResize() {
    let row = this.resizedRowNum;
    if (row != null) {
      let hg = this._getHypergrid(),
          height = hg.getRowHeight(row);
      API.setRowHeight(row+1, height);
      // row index on DB is 1-indexed, while for hypergrid it's 0-indexed.
      this.resizedRowNum = null;
    }
  }

  // note: evt.gridCell from the mouse-click functions from model (hg.getBehavior()) seem to be 1-indexed
  // (the coordinates of the top left cell are  (1,1) rather than (0,0)). The below two functions are only
  // assumed to work on mouse functions on model, NOT for e.g. fin-double-click events where the top left cell is (0,0). Umm......
  _clickedCellIsInColumnHeader(clickedCell: HGPoint): boolean {
    return (clickedCell.x >= 1 && clickedCell.y <= 0); // == 0? ==-1? not entirely sure
  }

  _clickedCellIsInRowHeader(clickedCell: HGPoint): boolean {
    return (clickedCell.x <= 0 && clickedCell.y >= 1); // == 0? ==-1? not entirely sure
  }

  _isLeftClick(e: HGMouseEvent): boolean { 
    return e.primitiveEvent.detail.primitiveEvent.which == 1; 
  }

  // expects that the current sheet has already been set
  getInitialData() {
    API.openSheet(SheetStateStore.getCurrentSheet());
    ActionCreator.scroll(this.getViewingWindow());
  }

  /*************************************************************************************************************************/
  // Hypergrid update display

  /* Called by eval pane's onChange method, when eval pane receives a change evt from the store */
  updateCellValues(clientCells: Array<ASCellObject>) {
    let model = this._getBehavior(),
        self = this;
    // Update the hypergrid values
    clientCells.forEach((c) => {
      let cellSheetId = c.cellLocation.sheetId,
          gridCol = c.cellLocation.index.col-1, // hypergrid starts indexing at 0
          gridRow = c.cellLocation.index.row-1, // hypergrid starts indexing at 0
          display = U.Render.showValue(c.cellValue);
      model.setValue(gridCol, gridRow, display.toString());
      // Update our list of overlays if we have an image
      self.addCellSourcedOverlay(c);
    });

    model.changed(); // causes hypergrid to show updated values
    CellStore.resetLastUpdatedCells();
  }

  /*************************************************************************************************************************/
  // Dealing with image creation and updating

  /*
  Given a cell that has a ValueImage tag, get the corresponding overlay from the info in that cell. This involves
  extracting out offset and size information from the cell. We also need to take scrolling into account to render the
  picture initially in the correct place. We use Hypergrid methods to return an Overlay object.
  Note that we don't account for scroll here. The scroll state is passed as a prop to Overlay, which will deal with the scroll
  */
  getImageOverlayForCell(cell: ASCellObject): ?ASOverlaySpec {
    let {col, row} =  cell.cellLocation.index,
        p =  finRect.point.create(col, row),
        point = this._getHypergrid().getBoundsOfCell(p).origin;
    /*
    Define default parameters, and fill them in with values from the cell information.
    If the image was resized or dragged, its metadata would have been modified and updateCellValues would be called,
    which calls this function. Here, we produce the up-to-date overlay based on current offsets and size
    */
    let ct = cell.cellProps, imageWidth = 300, imageHeight = 300, imageOffsetX = 0, imageOffsetY = 0;
    for (var i = 0 ; i < ct.length; i++) {
      if (ct[i].tag === "ImageData") {
        imageOffsetX = ct[i].imageOffsetX;
        imageOffsetY = ct[i].imageOffsetY;
        imageWidth   = ct[i].imageWidth;
        imageHeight  = ct[i].imageHeight;
      }
    }
    if (cell.cellValue.tag !== "ValueImage") {
      return null;
    }  else {
      let imagePath = cell.cellValue.imagePath;
      // Return the overlay spec, and note that the overlay shouldn't be in view if the point isn't
      // Compute the overlay element. The "draggable=false" is needed for a silly HTML5 reason.
      let imageSrc = Constants.getBackendUrl('http', Constants.BACKEND_STATIC_PORT) + "/images/" + imagePath;
      return {
        id: U.Render.getUniqueId(),
        renderElem: (style) => {
          return (<image src={imageSrc} draggable="false" style={style} alt="Error rendering image." />);
        },
        initWidth: imageWidth,
        initHeight: imageHeight,
        offsetX: imageOffsetX,
        offsetY: imageOffsetY,
        left: point.x,
        top:  point.y,
        loc: cell.cellLocation
      };
    }
  }

  /*
  As part of our state, we store a list of overlay objects. When a cell produces an overlay, we want to update this list.
  If there's already an overlay at that location, we want to replace it with the new one (so that propagation works!).
  In particular, if a cell with an overlay is deleted, the newOverlay will be null (nothing added) and the old one will be deleted.
  That location-based update and state change is done here.
  */
  addCellSourcedOverlay(cell: ASCellObject) {
    let imageOverlay = this.getImageOverlayForCell(cell);
    if (imageOverlay === null || imageOverlay === undefined) return;
    this.addOverlay(imageOverlay, cell);
  }

  addOverlay(newOverlay: ASOverlaySpec, cell?: ASCellObject) {
    let overlays = this.state.overlays,
        locs = catMaybes(overlays.map((o) => o.loc));

    locs.forEach((loc, i) => {
      if (cell !== null && cell !== undefined) {
        if (U.Render.locEquals(loc, cell.cellLocation)) {
          overlays.splice(i,1);
        }
      }
    });

    overlays.push(newOverlay);
    this.setState({overlays: overlays});
  }

  _onOverlaysChange() {
    let overlays = OverlayStore.getAll();
    overlays.forEach((overlay) => {
      this.addOverlay(overlay);
    });
  }


  /*************************************************************************************************************************/
  // Hypergrid methods for updating selection, focus, scrolling

  repaint() {
    this._getHypergrid().repaint();
  }

  setFocus() {
    this._getHypergrid().takeFocus();
  }

  // do not call before polymer is ready.
  select(unsafeSelection: ASSelectionObject, shouldScroll: boolean = true) {
    logDebug("Spreadsheet select start");

    // unsafe if it references values <= 0.
    let safeSelection = U.Location.getSafeSelection(unsafeSelection);
    let {tl, br} = safeSelection.range;
    let {col, row} = safeSelection.origin;

    let oldSel = SelectionStore.getActiveSelection();
    // make selection
    let hg = this._getHypergrid(),
        originIsCorner = U.Location.originIsCornerOfSelection(safeSelection),
        c, r, dC, dR;

    if (originIsCorner) {
      let flipC = (col == br.col) ? -1 : 1,
          flipR = (row == br.row) ? -1 : 1;
      c = col - 1;
      r = row - 1;
      dC = (br.col - tl.col) * flipC;
      dR = (br.row - tl.row) * flipR;
    } else {
      // sort of a hack. This will incorrectly change the location of the origin.
      // known source of bugs. E.g., ctrl+space on C3 and Ctrl+Up will take you to A1, not C1. (Alex 11/4)
      c = tl.col - 1;
      r = tl.row - 1;
      dC = br.col - tl.col;
      dR = br.row - tl.row;
    }

    hg.takeFocus();
    hg.clearSelections();
    hg.select(c, r, dC, dR);

    // set mousedown
    // hypergrid sucks -- doesn't set the mouse focus automatically
    // with select, so we have to do it ourselves.
    let myDown = finRect.point.create(c,r),
        myExtent = finRect.point.create(dC, dR);
    hg.setMouseDown(myDown);
    hg.setDragExtent(myExtent);

    let win = this.getViewingWindow().range;
    // set scroll
    if (shouldScroll) {
      let scroll = this._getNewScroll(oldSel, safeSelection);
      this.scrollTo(scroll.x, scroll.y);
    }
    this.repaint();
    this.props.onSelectionChange(safeSelection);
  }

  rowVisible(loc: NakedIndex): boolean {
    let vWindow = this.getViewingWindow();
    return (vWindow.range.tl.row <= loc.row && loc.row <= vWindow.range.br.row);
  }

  columnVisible(loc: NakedIndex): boolean {
    let vWindow = this.getViewingWindow();
    return (vWindow.range.tl.col <= loc.col && loc.col <= vWindow.range.br.col);
  }

  scrollVForBottomEdge(row: number): number {
    let hg = this._getHypergrid();
    let vWindow = this.getViewingWindow();
    return hg.getVScrollValue() + row - vWindow.range.br.row + 2;
  }

  scrollVForTopEdge(row: number): number {
    return row - 1;
  }

  scrollHForRightEdge(col: number): number {
    let hg = this._getHypergrid();
    let vWindow = this.getViewingWindow();
    return hg.getHScrollValue() + col - vWindow.range.br.col;
  }

  scrollHForLeftEdge(col: number): number {
    return col - 1;
  }

  _getNewScroll(oldSel: ?ASSelectionObject, newSel: ASSelectionObject): HGPoint {
    let hg = this._getHypergrid();
    let {
      range: {tl, br},
      origin: {col, row}
    } = newSel;

    let win = this.getViewingWindow();
    let scrollH = hg.getHScrollValue(), scrollV = hg.getVScrollValue();

    let oldOrigin, oldRange, oldTl, oldBr;
    if (oldSel) {
      let {origin: oldOrigin, range: oldRange} = oldSel;
      let {tl: oldTl, br: oldBr} = oldRange;

      // I think this code is a little hacky; I haven't thought this through deeply to ensure that
      // it works in all cases. It does work for ctrl shift arrows and ctrl arrows though. (Alex 11/3/15)

      if (oldOrigin) {
        if (U.Render.simpleIndexEquals(oldOrigin, newSel.origin)) {
          if (this.rowVisible(oldTl) && !this.rowVisible(tl)) {
            scrollV = this.scrollVForTopEdge(tl.row);
          } else if (this.rowVisible(oldBr) && !this.rowVisible(br)) {
            scrollV = this.scrollVForBottomEdge(br.row); // for some reason it works better with the + 2
          }

          if (this.columnVisible(oldTl) && !this.columnVisible(tl)) {
            scrollH = this.scrollHForLeftEdge(tl.col);
          } else if (this.columnVisible(oldBr) && !this.columnVisible(br)) {
            scrollH = this.scrollHForRightEdge(br.col);
          }
        } else {
          if (col < win.range.tl.col) {
            scrollH = this.scrollHForLeftEdge(col)
          } else if (col > win.range.br.col) {
            scrollH = this.scrollHForRightEdge(col);
          }

          if (row < win.range.tl.row) {
            scrollV = this.scrollVForTopEdge(row);
          } else if (row > win.range.br.row) {
            scrollV = this.scrollVForBottomEdge(row);
          }
        }
      }
    }

    return {x: scrollH, y: scrollV};
  }

  shiftSelectionArea(dc: number, dr: number) {
    let sel = SelectionStore.getActiveSelection();
    if (! sel) {
      logError('Trying to shift null selection');
      return;
    }

    let origin = {row: sel.origin.row + dr, col: sel.origin.col + dc};
    let range = {tl: origin, br: origin};
    this.select({range: range, origin: origin});
  }

  scrollTo(x: number, y: number) {
    let hg = this._getHypergrid();
    if (hg.getHScrollValue() != x || hg.getVScrollValue() != y) {
      hg.setVScrollValue(y),
      hg.setHScrollValue(x);
      ActionCreator.scroll(this.getViewingWindow());
    }
  }

  /*************************************************************************************************************************/
  // Handling events

  _onKeyDown(e: SyntheticKeyboardEvent) {
    logDebug("GRID KEYDOWN", e);
    e.persist(); // prevent react gc
    if (ShortcutUtils.gridShouldDeferKey(e)) {
      KeyUtils.killEvent(e);
      let userIsTyping = ExpStore.getUserIsTyping(),
          clickType    = ExpStore.getClickType();
      logDebug("CLICK TYPE ", clickType);
      if (ShortcutUtils.gridShouldAddToTextbox(userIsTyping, e)) {
        // Need to update the editor and textbox now via action creators
        logDebug("Grid key down going to AC");
        let [newStr, cursorPos] = KeyUtils.modifyTextboxForKey(e,
                                                  userIsTyping,
                                                  clickType,
                                                  ExpStore.getExpression(),
                                                  this.refs.textbox.editor);

        // if visible key and there was a last cell ref, move the selection back to the origin
        let activeSelection = SelectionStore.getActiveSelection();
        if (!! activeSelection && ExpStore.getLastRef() !== null) {
          this.select(activeSelection);
        }
        this.props.hideToast();

        ExpActionCreator.handleGridChange(newStr, cursorPos);
      } else {
        // Try shortcuts
        logDebug("Grid key down, trying shortcut");
        ShortcutUtils.tryGridShortcut(e);
      }
    } else if (KeyUtils.isNavKey(e)) { // nav key from grid
      let activeSelection = SelectionStore.getActiveSelection();
      if (!activeSelection) {
        logDebug('No selection');
        return;
      }

      let {range, origin} = activeSelection;
      logDebug("ACTIVE SEL AFTER NAV KEY", origin);
      if (KeyUtils.isPureArrowKey(e) && !U.Location.isIndex(range)) {
        logDebug("MANUALLY HANDLING NAV KEY");
        KeyUtils.killEvent(e);
        let newOrigin = KeyUtils.shiftIndexByKey(e, origin);
        this.select({range: {tl: newOrigin, br: newOrigin}, origin: newOrigin});
      }
      this.props.onNavKeyDown(e);
    }
  }

  _onKeyUp(e: SyntheticKeyboardEvent) {
    e.persist();
  }

  _onFocus(e: SyntheticEvent) {
    /*
    Only sometimes, for reasons I don't fully understand
    (might have something to do with position props) updateTextBox causes onFocus to fire in grid
    If this happens, manually put the focus back in the textbox
    -- RITESH
    */
    logDebug("Grid on focus");
    if (ExpStore.getClickType() === Constants.ClickType.DOUBLE_CLICK) {
      this.props.setFocus('textbox');
    } else {
      this.props.setFocus('grid');
    }
  }

  _restoreFocus() {
      this.props.setFocus(SheetStateStore.getFocus());
  }

  /*************************************************************************************************************************/
  // Respond to change evt from ExpStore

  _onExpressionChange() {
    let xpChangeOrigin = ExpStore.getXpChangeOrigin(),
        xpStr = ExpStore.getExpression(),
        cursorPos = ExpStore.getCursorPos();
    if (xpChangeOrigin != null) {
      logDebug("Grid caught exp update of_type: " +  xpChangeOrigin);
    } else {
      logDebug("Grid caught exp udpate of_type: null");
    }
    switch(xpChangeOrigin) {
      case Constants.ActionTypes.TEXTBOX_CHANGED:
        Render.setShouldRenderSquareBox(false);
        // no square box while typing
        break;
      case Constants.ActionTypes.EDITOR_CHANGED:
      case Constants.ActionTypes.GRID_KEY_PRESSED:
        Render.setShouldRenderSquareBox(false);
        this.repaint();
        this.refs.textbox.updateTextBox(xpStr, cursorPos);
        break;
      // hide textbox, if focus not already in grid, put it there
      case Constants.ActionTypes.NORMAL_SEL_CHANGED:
        this.refs.textbox.hideTextBox(xpStr);
        this.props.setFocus('grid');
        break;
      case Constants.ActionTypes.BACKEND_UPDATED_AND_CELLS_CHANGED:
        break;
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_GRID:
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_EDITOR:
        this.refs.textbox.updateTextBox(xpStr);
        break;
      case Constants.ActionTypes.ESC_PRESSED:
        this.refs.textbox.updateTextBox(xpStr);
        this.refs.textbox.hideTextBox();
        break;
      // put focus on grid on get
      case Constants.ActionTypes.GOT_UPDATED_CELLS:
        this.props.setFocus('grid');
        break;
      case Constants.ActionTypes.LANGUAGE_TOGGLED:
        this.refs.textbox.updateLanguage();
        break;
      default:
        break;
    }
  }

  _onBarPropsChange() {
    let dims = BarStore.getLastUpdatedBarsDimensions(),
        hg = this._getHypergrid(),
        model = hg.getBehavior(),
        defaultColumnWidth = hg.resolveProperty('defaultColumnWidth'),
        defaultRowHeight = hg.resolveProperty('defaultRowHeight');

    // columns/rows in backend are 1-indexed, hypergrid's are 0-indexed.
    dims['ColumnType'].map(([ind, width]) => model._setColumnWidth(ind-1, width || defaultColumnWidth));
    dims['RowType'].map(([ind, height]) => model.setRowHeight(ind-1, height || defaultRowHeight));
  }

  /*************************************************************************************************************************/
  // Render

  render(): ReactElement {
    let {behavior, width, height} = this.props; //should also have onReady

    const outerStyle = {
      position: 'relative',
      display: 'flex',
      overflow: 'hidden',
      cursor: this.state.cursorStyle,
      flexGrow: 1,
      flexShrink: 1,
      flexBasis: 'auto'
    };

    const sheetContainerStyle = {
      display: 'flex',
      flexDirection: 'column',
      flexGrow: 1,
      flexShrink: 1,
      flexBasis: 'auto'
    };

    const sheetStyle = {
      flexGrow: 1,
      flexShrink: 1,
      width: '100%',
      height: '100%',
      cursor: 'auto'
    };

    let behaviorElement;
    let self = this;
    switch (behavior) {
      case 'json':
        behaviorElement = <fin-hypergrid-behavior-json />;
        break;
      case 'default':
        behaviorElement = <fin-hypergrid-behavior-default />;
        break;
    }

    return (
      // NOTE: the 50px is for the scrollbar to show up.
      <Dropzone onDrop={this.props.onFileDrop} disableClick={true} style={outerStyle}>
        <div ref="sheet" style={sheetContainerStyle} >
          <fin-hypergrid
            style={sheetStyle}
            ref="hypergrid"
            onKeyDown={this._onKeyDown.bind(this)}
            onKeyUp={this._onKeyUp.bind(this)}
            onFocus={this._onFocus.bind(this)}>
              {behaviorElement}
          </fin-hypergrid>

          {this.state.overlays.map((overlay) =>
            <ASOverlay key={overlay.id}
                       overlay={overlay}
                       scrollPixels={self.state.scrollPixels}
                       isVisible={self.isVisible} />
          )}

          <ASRightClickMenu ref="rightClickMenu"
                            restoreFocus={this._restoreFocus.bind(this)}/>

          <Textbox
            ref="textbox"
            scroll={self.state.scroll}
            onDeferredKey={this.props.onTextBoxDeferredKey}
            hideToast={this.props.hideToast}
            position={this.getTextboxPosition.bind(this)}
            setFocus={this.props.setFocus} />

        </div>
      </Dropzone>
    );
  }
}

// TODO: is behavior actually needed?
ASSpreadsheet.defaultProps = {
  behavior: 'default',
  onReady() { }
}

ASSpreadsheet.propTypes = {
  onSelectionChange: React.PropTypes.func.isRequired,
  onNavKeyDown: React.PropTypes.func.isRequired,
  setFocus: React.PropTypes.func.isRequired
}
