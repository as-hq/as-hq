/* @flow */

import type {
  ASOverlaySpec
} from '../types/Overlay';

import type {
  ASCursorStyle,
} from '../types/State';

import type {
  Callback
} from '../types/Base';

import type {
  Dimensions
} from '../types/Hypergrid';

import type {
  PXRectangle
} from '../types/Render';

import type { StoreLink } from '../types/React';
import type { StoreToken } from 'flux';

import {logDebug, logError} from '../AS/Logger';
import {catMaybes} from '../AS/Maybe';

import _ from 'lodash';
import invariant from 'invariant';

import React from 'react';
import {findDOMNode} from 'react-dom';

import GridActions from '../actions/ASGridActionCreators';
import ExpressionActions from '../actions/ASExpressionActionCreators';

import API from '../actions/ASApiActionCreators';

import CellStore from '../stores/ASCellStore';
import WorkbookStore from '../stores/ASWorkbookStore';
import FindStore from '../stores/ASFindStore';
import BarStore from '../stores/ASBarStore.js';
import OverlayStore from '../stores/ASOverlayStore';
import FocusStore from '../stores/ASFindStore';
import GridStore from '../stores/ASGridStore';
import ConfigStore from '../stores/ASConfigurationStore';
import ExpressionStore from '../stores/ASExpressionStore';
import FocusActions from '../actions/ASFocusActionCreators';

import U from '../AS/Util';
const {
  Conversion: TC,
  Key: KeyUtils,
  Shortcut: ShortcutUtils
} = U;

import ASPoint from '../classes/ASPoint';
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
  cursorStyle: ASCursorStyle;
};

class ASSpreadsheet extends React.Component {
  static defaultProps = {};
  props: Props;
  state: State;

  /*************************************************************************************************************************/
  // Non-rendering state
  $storeLinks: Array<StoreLink>;
  _grid: any;
  _onGridFocus: Callback<SyntheticEvent>;
  _cellStoreListener: StoreToken;
  _configStoreListener: StoreToken;

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
      cursorStyle: 'auto'
    };

    // Overridden by the Focusable HOC
    this._onGridFocus = (e) => { };
  }

  componentDidMount() {
    U.React.addStoreLinksWithoutForceUpdate(this, [
      { store: BarStore, listener: () => this._onBarPropsChange() },
    ]);

    this._cellStoreListener = CellStore.addListener(() =>
      this._grid.getBehavior().changed()
    );

    // apply hypergrid customizations when its canvas is ready.
    document.addEventListener('fin-ready', () => {
      this._initHypergrid();

      // GridStore is the source of truth for hypergrid-related parameters,
      // but Hypergrid itself is not in scope in the store. The current
      // workaround is to expose this single function in the global scope.
      // The alternative is to enumerate the set of all cases grid dimensions
      // might change, then wait for grid to update internal state, then fire a
      // GRID_RESIZED action. That was the solution we had before, and it sucked.
      // This method provides correct information all the time, despite being ugly.
      // #8020.
      window.getGridDimensions = () => this._getDimensions();

      window.addEventListener('focus', () => {
        this._grid.repaint();
      });
      // due to asynchronous grid mount, refresh all store-obtained properties
      this.$storeLinks.forEach(({listener}) => listener());
    });

    document.addEventListener('grid-repaint', () => {
      this._grid.repaint();
    });

  }

  componentWillUnmount() {
    U.React.removeStoreLinks(this);
    this._cellStoreListener.remove();
    this._configStoreListener.remove();
  }

  /*************************************************************************************************************************/
  // Handle mouse events by overriding hypergrid default

  drawDraggedSelection(dragOrigin: ASIndex, selRange: ASRange, targetX: number, targetY: number) {
    const dX = targetX - dragOrigin.col;
    const dY = targetY - dragOrigin.row;
    const range = selRange.shift({ dY, dX });
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
  // Hypergrid initialization

  finishColumnResize() {
    const col = this.resizedColNum;
    if (col != null) {
      const width = this._grid.getColumnWidth(col);
      API.setColumnWidth(col+1, width);
      // column index on DB is 1-indexed, while for hypergrid it's 0-indexed.
      this.resizedColNum = null;
    }
  }

  finishRowResize() {
    const row = this.resizedRowNum;
    if (row != null) {
      const height = this._grid.getRowHeight(row);
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

  /*************************************************************************************************************************/
  // Render

  render(): any { // ::ALEX::
    const {height} = this.props;
    const {cursorStyle} = this.state;

    return (
      <Dropzone onDrop={files => API.importAlphaSheets(files[0])}
                disableClick={true}
                style={{cursor: cursorStyle, ...styles.root}}>

        <ASOverlayController
          getPixelCoordinates={idx => this._getPixelCoordinates(idx)}
          />

        <div style={styles.sheetContainer} >
          <Textbox getPixelCoordinates={idx => this._getPixelCoordinates(idx)} />

          <fin-hypergrid
            ref={elem => this._grid = elem}
            style={styles.sheet}
            onKeyDown={evt => this._onKeyDown(evt)}
            onFocus={evt => this._onGridFocus(evt)}
            onMouseEnter={() => FocusActions.hover(name)}
            onMouseLeave={() => FocusActions.unhover(name)}
          >
            <fin-hypergrid-behavior-default />
          </fin-hypergrid>

          <ASRightClickMenu ref="rightClickMenu" />


        </div>
      </Dropzone>
    );
  }

  /*************************************************************************************************************************/
  // Private getters
  // do not call any of these before polymer is ready.

  _getPixelCoordinates({col, row}: ASIndex): PXRectangle {
    if (!! this._grid) {
      // get extent from getBoundsOfCell
      const scroll = this._getScroll();
      const point = finRect.point.create(col - scroll.x + 1, row - scroll.y + 1);
      const {extent} = this._grid.getBoundsOfCell(point);

      const fixedPaddingX = this._grid.getFixedColumnWidth(0);
      const fixedPaddingY = this._grid.getFixedRowHeight(0);

      const sum = (arr) => arr.reduce((acc, cur) => acc + cur, 0);

      // directional sum from i=[start, end) of iterator(i)
      function liftDirectionalDiff(
        iterator: (val: number) => number,
        start: number,
        end: number
      ): number {
        if (end >= start) {
          return sum(_.range(start, end).map(iterator));
        } else {
          return -sum(_.range(end, start).map(iterator));
        }
      }

      // get origin from summing
      // scroll.x ... col
      // scroll.y ... row

      const left = liftDirectionalDiff(
        (x) => this._grid.getColumnWidth(x),
        scroll.x - 1, col - 1
      ) + fixedPaddingX;

      const top = liftDirectionalDiff(
        (y) => this._grid.getRowHeight(y),
        scroll.y - 1, row - 1
      ) + fixedPaddingY;

      const origin = {x: left, y: top};

      return {origin, extent};
    } else {
      console.error('no grid found in getTextboxPosition!');
      return U.Hypergrid.defaultRectangle();
    }
  }

  _getCoordsFromMouseEvent(evt: HGMouseEvent): HGPoint {
    const {x, y} = evt.mousePoint;
    const point = finRect.point.create(evt.gridCell.x, evt.gridCell.y);
    const {origin} = this._grid.getBoundsOfCell(point);
    const pX = origin.x + x;
    const pY = origin.y + y;
    return {x: pX, y: pY};
  }

  _getBehavior(): HGBehaviorElement {
    return this._grid.getBehavior();
  }

  _getScroll(): ASPoint {
    const hg = this._grid;
    return new ASPoint({
      x: hg.hScrollValue + 1,
      y: hg.vScrollValue + 1
    });
  }

  _getDimensions(): Dimensions {
    return {
      width: this._grid.getViewableColumns() - 1, // hypergrid is actually just wrong.
      height: this._grid.getViewableRows() - 1
    };
  }

  _getSelection(): ASSelection {
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

  /*************************************************************************************************************************/
  // Private setters
  // do not call any of these before polymer is ready.

  _initHypergrid() {
    hgPatches.forEach((patch) => { patch(this); });
    GridActions.initialize();
  }

  _select(selection: ASSelection) {
    const {
      range: {tl, br},
      origin: {col, row}
    } = selection;

    const oldSelection = GridStore.getLastActiveSelection();
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

    // set mousedown manually, since hypergrid doesn't do it for you.
    const myDown = finRect.point.create(c,r);
    const myExtent = finRect.point.create(dC, dR);
    this._grid.setMouseDown(myDown);
    this._grid.setDragExtent(myExtent);

    this._grid.repaint();
  }

  _scrollTo({x, y}: ASPoint) {
    const hg = this._grid;
    const [x_hg, y_hg] = [x-1, y-1]; // hypergrid is off-by-one.
    if (hg.getHScrollValue() != x_hg || hg.getVScrollValue() != y_hg) {
      hg.setVScrollValue(y_hg),
      hg.setHScrollValue(x_hg);
    }
  }

  /*************************************************************************************************************************/
  // Event handlers

  _onKeyDown(e: SyntheticKeyboardEvent) {
    e.persist(); // prevent react gc
    GridActions.executeKey(e);
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

const name = 'grid';
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
