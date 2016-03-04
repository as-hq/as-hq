/* @flow */

import type {
  ASOverlaySpec
} from '../types/Overlay';

import type {
  ASCursorStyle,
  ASViewingWindow,
} from '../types/State';

import type {
  Callback
} from '../types/Base';

import type {
  PXRectangle
} from '../types/Render';

import type { StoreLink } from '../types/React';
import type { StoreToken } from 'flux';

import {logDebug, logError} from '../AS/Logger';
import {catMaybes} from '../AS/Maybe';

import _ from 'lodash';
// $FlowFixMe
import invariant from 'invariant';

import React from 'react';
import {findDOMNode} from 'react-dom';

import SpreadsheetActions from '../actions/ASSpreadsheetActionCreators';
import ExpressionActions from '../actions/ASExpressionActionCreators';

import API from '../actions/ASApiActionCreators';

import CellStore from '../stores/ASCellStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import SelectionStore from '../stores/ASSelectionStore';
import FindStore from '../stores/ASFindStore';
import BarStore from '../stores/ASBarStore.js';
import OverlayStore from '../stores/ASOverlayStore';
import FocusStore from '../stores/ASFindStore';
import ConfigStore from '../stores/ASConfigurationStore';
import ExpressionStore from '../stores/ASExpressionStore';

import U from '../AS/Util';
let {
  Conversion: TC,
  Key: KeyUtils,
  Shortcut: ShortcutUtils
} = U;

import ASCell from '../classes/ASCell';
import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

import Constants from '../Constants';
import Render from '../AS/Renderers';

import Focusable from './transforms/Focusable.jsx';
import ASRightClickMenu from './basic-controls/ASRightClickMenu.jsx';
import Textbox from './Textbox.jsx';
import ASOverlayController from './overlays/ASOverlayController.jsx';

// $FlowFixMe: this module clearly exists, but flow can't find it??!
import Dropzone from 'react-dropzone';

import hgPatches from '../hg-patches/index';

let finRect: HGRectangleElement = (document.createElement('fin-rectangle'): any);

type Props = {
  onFileDrop: Callback<Array<File>>;
  height?: string;
};

type State = {
  scroll: HGPoint;
  scrollPixels: HGPoint;
  cursorStyle: ASCursorStyle;
};

class ASSpreadsheet extends React.Component<{}, Props, State> {
  /*************************************************************************************************************************/
  // Non-rendering state
  $storeLinks: Array<StoreLink>;
  _grid: any;
  _onGridFocus: Callback<SyntheticEvent>;
  _configStoreListener: StoreToken;
  _cellStoreListener: StoreToken;

  mousePosition: ?HGPoint;
  mouseDownInBox: boolean;
  dragSelectionOrigin: ?ASIndex;

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

  constructor(props: Props) {
    super(props);
    this.$storeLinks = [];

    this.mousePosition = null;
    this.mouseDownInBox = false;
    this.dragSelectionOrigin = null;


    this.resizedColNum = null;
    this.resizedRowNum = null;

    this.gridProperties = {
      editorActivationKeys: [],
      scrollbarHoverOff: 'visible',
      columnAutosizing: false,
      defaultFixedColumnWidth: 35,
      defaultColumnWidth: 100
    };

    this.state = {
      // keep scroll values in state so overlays autoscroll with grid
      scroll: { x: 0, y: 0 },
      scrollPixels: { x: 0, y: 0},
      cursorStyle: 'auto'
    };

    // Overridden by the Focusable HOC
    this._onGridFocus = (e) => { };
  }

  componentDidMount() {
    U.React.addStoreLinksWithoutForceUpdate(this, [
      { store: BarStore, listener: () => this._onBarPropsChange() },
    ]);

    this._cellStoreListener = CellStore.addListener(() => {
      this._grid.getBehavior().changed();
    });
    this._configStoreListener = ConfigStore.addListener(() =>
      this._grid.repaint()
    );

    // apply hypergrid customizations when its canvas is ready.
    document.addEventListener('fin-ready', () => {
      this.initHypergrid();
      this.pullInitialData();
    });
  }

  initHypergrid() {
    hgPatches.forEach((patch) => { patch(this); });
    SpreadsheetActions.initialize();
  }

  componentWillUnmount() {
    U.React.removeStoreLinks(this);
    this._cellStoreListener.remove();
    this._configStoreListener.remove();
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

  drawDraggedSelection(dragOrigin: ASIndex, selRange: ASRange, targetX: number, targetY: number) {
    let dX = targetX - dragOrigin.col,
        dY = targetY - dragOrigin.row;
    let range = selRange.shift({ dr: dY, dc: dX });
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

  getSelectionArea(): ASSelection {
    const hg = this._grid;
    const [selection] = hg.getSelectionModel().getSelections();
    if (!! selection) {
      const ul = selection.origin;
      const range = {
        tl: {row:  ul.y + 1,
             col:  ul.x + 1},
        br: {row: ul.y + selection.height() + 1,
             col: ul.x + selection.width() + 1}
      };
      const sel = {
        range: range,
        origin: {row: ul.y + 1, col: ul.x + 1}
      };
      return new ASSelection(sel);
    } else {
      return ASSelection.defaultSelection();
    }
  }

  getScroll(): HGPoint {
    let hg = this._grid;
    return {x: hg.hScrollValue, y: hg.vScrollValue};
  }

  getViewingWindow(): ASViewingWindow {
    let hg = this._grid,
        [vs, hs] = [hg.vScrollValue, hg.hScrollValue],
        [cols, rows] = [hg.getVisibleColumns(), hg.getVisibleRows()];
        let colLength = cols.length, rowLength = rows.length;
        // This might fail on the initial load, since getVisibleColumns() and
        // getVisibleRows() might return nothing, ergo the below hack.
        if (colLength == 0) colLength = 20;
        if (rowLength == 0) rowLength = 30;
    return ASRange.fromNaked({
      tl: {
        row: vs + 1,
        col: hs + 1
      },
      br: {
        row: vs + rowLength - 1,
        col: hs + colLength - 1
      }
    });
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
    return this._grid.getVisibleRows().length;
  }

  /*************************************************************************************************************************/
  // Hypergrid initialization

  finishColumnResize() {
    let col = this.resizedColNum;
    if (col != null) {
      let hg = this._grid,
          width = hg.getColumnWidth(col);
      API.setColumnWidth(col+1, width);
      // column index on DB is 1-indexed, while for hypergrid it's 0-indexed.
      this.resizedColNum = null;
    }
  }

  finishRowResize() {
    let row = this.resizedRowNum;
    if (row != null) {
      let hg = this._grid,
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
  pullInitialData() {
    API.openSheet(SheetStateStore.getCurrentSheetId());
    // lazy-loading disabled (anand 2/15)
    // SpreadsheetActions.scroll(this.getViewingWindow());
  }


  /*************************************************************************************************************************/
  // Hypergrid methods for updating selection, focus, scrolling

  repaint() {
    this._grid.repaint();
  }

  // do not call before polymer is ready.
  select(selection: ASSelection, shouldScroll: boolean = true) {
    const {
      range: {tl, br},
      origin: {col, row}
    } = selection;

    const oldSelection = SelectionStore.getLastActiveSelection();
    const originIsCorner = selection.originIsCorner();

    let c, r, dC, dR;
    if (originIsCorner) {
      const flipC = (col == br.col) ? -1 : 1;
      const flipR = (row == br.row) ? -1 : 1;
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

    this._grid.clearSelections();
    this._grid.select(c, r, dC, dR);

    // set mousedown
    // hypergrid sucks -- doesn't set the mouse focus automatically
    // with select, so we have to do it ourselves.
    const myDown = finRect.point.create(c,r);
    const myExtent = finRect.point.create(dC, dR);
    this._grid.setMouseDown(myDown);
    this._grid.setDragExtent(myExtent);

    const win = this.getViewingWindow();
    // set scroll
    if (shouldScroll) {
      const scroll = this._getViewingWindowFromOldAndNewSelections(oldSelection, selection);
      this.scrollTo(scroll.x, scroll.y);
    }
    this.repaint();
  }

  rowVisible(loc: ASIndex): boolean {
    const vWindow = this.getViewingWindow();
    return (vWindow.tl.row <= loc.row && loc.row <= vWindow.br.row);
  }

  columnVisible(loc: ASIndex): boolean {
    const vWindow = this.getViewingWindow();
    return (vWindow.tl.col <= loc.col && loc.col <= vWindow.br.col);
  }

  scrollVForBottomEdge(row: number): number {
    let hg = this._grid;
    let vWindow = this.getViewingWindow();
    return hg.getVScrollValue() + row - vWindow.br.row;
  }

  scrollVForTopEdge(row: number): number {
    return row - 1;
  }

  scrollHForRightEdge(col: number): number {
    let hg = this._grid;
    let vWindow = this.getViewingWindow();
    return hg.getHScrollValue() + col - vWindow.br.col;
  }

  scrollHForLeftEdge(col: number): number {
    return col - 1;
  }

  _getViewingWindowFromOldAndNewSelections(oldSel: ?ASSelection, newSel: ASSelection): HGPoint {
    let hg = this._grid;
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
        if (oldOrigin.equals(newSel.origin)) {
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
          if (col < win.tl.col) {
            scrollH = this.scrollHForLeftEdge(col)
          } else if (col > win.br.col) {
            scrollH = this.scrollHForRightEdge(col);
          }

          if (row < win.tl.row) {
            scrollV = this.scrollVForTopEdge(row);
          } else if (row > win.br.row) {
            scrollV = this.scrollVForBottomEdge(row);
          }
        }
      }
    }

    return {x: scrollH, y: scrollV};
  }

  // when you press enter etc after eval, the selection origin shifts and the
  // rest of the selection gets reset to a single cell
  shiftAndResetSelection(byCoords: ({ dr: number; dc: number; })) {
    const {origin} = SelectionStore.getActiveSelection();
    this.select(origin.shift(byCoords).toSelection());
  }

  scrollTo(x: number, y: number) {
    let hg = this._grid;
    if (hg.getHScrollValue() != x || hg.getVScrollValue() != y) {
      hg.setVScrollValue(y),
      hg.setHScrollValue(x);
      // Lazy loading disabled (anand 2/15)
      // SpreadsheetActions.scroll(this.getViewingWindow());
    }
  }

  /*************************************************************************************************************************/
  // Render

  render(): ReactElement {
    const {height} = this.props;
    const {scrollPixels, scroll, cursorStyle} = this.state;

    return (
      <Dropzone onDrop={files => API.import(files[0])}
                disableClick={true}
                style={{cursor: cursorStyle, ...styles.root}}>

        <ASOverlayController 
                    computeTopLeftPxOfLoc={(c, r) => this._computeTopLeftPxOfLoc(c, r)} />

        <div style={styles.sheetContainer} >

          <Textbox getPixelCoordinates={idx => this._getPixelCoordinates(idx)} />

          <fin-hypergrid
            ref={elem => this._grid = elem}
            style={styles.sheet}
            onKeyDown={evt => this._onKeyDown(evt)}
            onFocus={evt => this._onGridFocus(evt)}>
            <fin-hypergrid-behavior-default />
          </fin-hypergrid>

          <ASRightClickMenu ref="rightClickMenu" />


        </div>
      </Dropzone>
    );
  }

  /*************************************************************************************************************************/
  // Private getters

  _getPixelCoordinates({col, row}: ASIndex): PXRectangle {
    if (!! this._grid) {
      const scroll = this.state.scroll;
      const point = finRect.point.create(col - scroll.x, row - scroll.y);
      const {origin, extent} =  this._grid.getBoundsOfCell(point);
      return {origin, extent};

    } else {
      console.error('no grid found in getTextboxPosition!');
      return U.Hypergrid.defaultRectangle();
    }
  }

  // Given a column and row number, compute the location of that cell relative to the
  // top left of the grid. Used for positioning overlays. 
  // Note that the results can be negative. 
  // If this function turns out to be a bit slow (because it does some looping), 
  // some stuff can be moved into store, but this doesn't seem to be a bottleneck currently. 
  _computeTopLeftPxOfLoc(col: number, row: number) : {top: number, left: number} {
    let hg = this._grid,
        scrollX = hg.getHScrollValue(), 
        scrollY = hg.getVScrollValue();
    let bounds = hg.getBoundsOfCell(finRect.point.create(col, row));
    let scrollPxX = 0, scrollPxY = 0;
    for (let i = 1 ; i <= scrollX; i++) {
      scrollPxX += hg.getColumnWidth(i);
    }
    for (let j = 1 ; j <= scrollY; j++) {
      scrollPxY += hg.getRowHeight(j);
    }
    return {top: bounds.origin.y - scrollPxY, left: bounds.origin.x - scrollPxX};
  }

  _getBehavior(): HGBehaviorElement {
    return this._grid.getBehavior();
  }

  /*************************************************************************************************************************/
  // Event handlers

  _onKeyDown(e: SyntheticKeyboardEvent) {
    e.persist(); // prevent react gc

    // initiate 'buffered' editing (capture all keys until
    // textbox actually takes focus)
    if (KeyUtils.initiatesEditMode(e)) {
      KeyUtils.killEvent(e);
      ExpressionActions.startEditingBuffered(
        KeyUtils.keyToString(e)
      );
    }

    // if already editing, send the key to textbox, since
    // we must be in the middle of a focus transition.
    else if (ExpressionStore.isEditing()) {
      ExpressionActions.executeTextboxKey(e);
    }

    // let the window event handler take over.
    else if (KeyUtils.isCopyPasteType(e)) {
      return;
    }

    else {
      KeyUtils.killEvent(e);
      SpreadsheetActions.executeKey(e);
    }
  }

  _onBarPropsChange() {
    let dims = BarStore.getLastUpdatedBarsDimensions(),
        hg = this._grid,
        model = hg.getBehavior(),
        defaultColumnWidth = hg.resolveProperty('defaultColumnWidth'),
        defaultRowHeight = hg.resolveProperty('defaultRowHeight');

    // columns/rows in backend are 1-indexed, hypergrid's are 0-indexed.
    dims['ColumnType'].map(([ind, width]) => model._setColumnWidth(ind-1, width || defaultColumnWidth));
    dims['RowType'].map(([ind, height]) => model.setRowHeight(ind-1, height || defaultRowHeight));
  }

}

const styles = {
    root: {
      position: 'relative',
      display: 'flex',
      overflow: 'hidden',
      flexGrow: 1,
      flexShrink: 1,
      flexBasis: 'auto'
    },

    sheetContainer: {
      display: 'flex',
      flexDirection: 'column',
      flexGrow: 1,
      flexShrink: 1,
      flexBasis: 'auto'
    },

    sheet: {
      flexGrow: 1,
      flexShrink: 1,
      width: '100%',
      height: '100%',
      cursor: 'auto'
    }
};

export default ASSpreadsheet;
