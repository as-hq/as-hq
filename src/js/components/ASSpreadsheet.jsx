import {logDebug} from '../AS/Logger';

import _ from 'lodash';

import React from 'react';

import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import ExpActionCreator from '../actions/ASExpActionCreators';

import ShortcutUtils from '../AS/ShortcutUtils';
import API from '../actions/ASApiActionCreators';
import KeyUtils from '../AS/KeyUtils';

import Store from '../stores/ASEvaluationStore';
import FindStore from '../stores/ASFindStore';
import ExpStore from '../stores/ASExpStore';

import T from '../AS/Types';
import TC from '../AS/TypeConversions';
import Util from '../AS/Util';
import Constants from '../Constants';
import Render from '../AS/Render';

import ASRightClickMenu from './basic-controls/ASRightClickMenu.jsx';
import ASOverlay from './ASOverlay.jsx';
import Textbox from './Textbox.jsx'

import rowHeaderMenuItems from './menus/RowHeaderMenuItems.jsx';
import columnHeaderMenuItems from './menus/ColumnHeaderMenuItems.jsx';

let finRect = document.createElement('fin-rectangle');

export default React.createClass({

  /*************************************************************************************************************************/
  // Non-rendering state

  mousePosition: null,
  mouseDownInBox: false,
  dragSelectionOrigin: null,

  /*************************************************************************************************************************/
  // React methods

  propTypes: {
    onSelectionChange: React.PropTypes.func.isRequired,
    onTextBoxDeferredKey: React.PropTypes.func.isRequired,
    onNavKeyDown: React.PropTypes.func.isRequired,
    setFocus: React.PropTypes.func.isRequired
  },

  // TODO: do we actually need behavior??
  getDefaultProps() {
    return {
      behavior: 'default',
      language: Constants.Languages.Excel,
      onReady() { }
    };
  },

  getInitialState() {
    return {
      // keep scroll values in state so overlays autoscroll with grid
      scroll: { x:0, y:0 },
      overlays: [],
      cursorStyle: 'auto',
      selectionDraggable: false
    };
  },

  componentDidMount() {
    // Be able to respond to events from ExpStore
    ExpStore.addChangeListener(this._onExpressionChange);
    // Hypergrid initialization
    document.addEventListener('polymer-ready', () => {
      this.props.onReady();
      this.initialize();
      let self = this,
          hg = this._getHypergrid(),
          model = hg.getBehavior();
      this.getInitialData();

      let callbacks = ({
        /*
          Call onSelectionChange method in eval pane to deal with selection change
          Need to also figure out the expression to render in the editor
        */
        'fin-selection-changed': function (event) {
          ExpStore.setClickType(Constants.ClickType.CLICK);
          self.props.onSelectionChange(self.getSelectionArea());
        },
        'fin-scroll-x': function (event) {
          self.setState({scroll: self.getScroll()});
          if ((self.getScroll()).x % 20 === 0)
            ActionCreator.scroll(self.getViewingWindow());
        },
        'fin-scroll-y': function (event) {
          self.setState({scroll: self.getScroll()});
          if ((self.getScroll()).y % 20 === 0)
            ActionCreator.scroll(self.getViewingWindow());
        },
        'fin-double-click': function (event) {
          // TODO: double clicking inside blue box has diff behavior
          ExpStore.setClickType(Constants.ClickType.DOUBLE_CLICK);
          self.refs.textbox.updateTextBox(ExpStore.getExpression());
          Store.setFocus('textbox');
          self.props.setFocus('textbox');
        }
      });

      let externalCallbacks = {
        mouseup: ({which, x, y, offsetX, offsetY}) => {
          /* x, y: against the page, for rendering the dropdown */
          /* offsetX, offsetY: against hypergrid, for finding coordinates */

          if (which === 3) { // right click
            let {gridCell: {x: col, y: row}} =
              hg.renderer.getGridCellFromMousePoint({
                x: offsetX,
                y: offsetY
              });

            if (col != 0 || row != 0) { // right click on a row header
              this.refs.rightClickMenu.openAt(x, y,
                (col != 0)
                  ? columnHeaderMenuItems(col)
                  : rowHeaderMenuItems(row)
              );
            }
          }
        }
      };

      _.forEach(callbacks, (v, k) => {
        hg.addFinEventListener(k, v);
      });

      _.forEach(externalCallbacks, (v, k) => {
        hg.addEventListener(k, v);
      });

      hg.addGlobalProperties({
        defaultFixedColumnWidth: 35,
        defaultColumnWidth: 100,
        columnAutosizing: false
      });
    });
  },

  componentWillUnmount(){
    ExpStore.removeChangeListener(this._onExpressionChange);
  },

  /*************************************************************************************************************************/
  // Handle mouse events by overriding hypergrid default

  getCoordsFromMouseEvent(grid, evt) {
    let {x, y} = evt.mousePoint,
        point = finRect.point.create(evt.gridCell.x, evt.gridCell.y),
        {origin} = grid.getBoundsOfCell(point),
        pX = origin.x + x,
        pY = origin.y + y;
    return {x: pX, y: pY};
  },

  drawDraggedSelection(dragOrigin, selRange, targetX, targetY) {
    let dX = targetX - dragOrigin.col,
        dY = targetY - dragOrigin.row;
    let range = Util.offsetRange(selRange, dY, dX);
    Render.setDragRect(range);
  },

  // Is the mouse location inside a blue box
  insideBox(event){
   let {x,y} = event.primitiveEvent.detail.mouse,
       topLeftBox = Render.getTopLeftBox(),
       boxWidth   = Render.getBoxWidth();
    return Util.mouseLocIsContainedInBox(x,y,topLeftBox,boxWidth);
  },

  // Semi-recursive function via timeouts -- this is how hypergrid does it
  // Need to scroll even if no mouse event, but you're at the edge of the grid
  scrollWithDraggables(grid) {
    if (this.mouseDownInBox || this.dragSelectionOrigin !== null) {
      console.log("DEALING WITH SCROLLING");
      let {x,y} = this.mousePosition,
          b = grid.getDataBounds(),
          numFixedColumns = grid.getFixedColumnCount(),
          numFixedRows = grid.getFixedRowCount(),
          dragEndInFixedAreaX = x < numFixedColumns,
          dragEndInFixedAreaY = y < numFixedRows;
      let xOffset = 0,
          yOffset = 0;
      if (x > b.origin.x + b.extent.x) {
        xOffset = 1;
      } else if (x < b.origin.x){
        xOffset = -1;
      }
      if (y > b.origin.y + b.extent.y) {
        yOffset = 1;
      } else if (y < b.origin.y){
        yOffset = -1;
      }
      let dragCellOffsetX = dragEndInFixedAreaX ? 0 : xOffset,
          dragCellOffsetY = dragEndInFixedAreaY ? 0 : yOffset;
      if (xOffset !== 0 || yOffset !== 0){
        grid.scrollBy(xOffset, yOffset);
        grid.repaint();
      }
      // The below number affects scrolling rate, not sure what it should be
      setTimeout(this.scrollWithDraggables.bind(this, grid), 800);
    }
  },

  /*************************************************************************************************************************/
  // Default getter methods, relating to location/scrolling/selection

  _getHypergrid() {
    return React.findDOMNode(this.refs.hypergrid);
  },

  _getBehavior(){
    return this._getHypergrid().getBehavior();
  },

  getSelectionArea() {
    let hg = this._getHypergrid(),
        selection = hg.getSelectionModel().selections[0],
        ul = selection.origin,
        range = Util.orientRange({
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
  },

  getScroll() {
    let hg = this._getHypergrid();
    return {x: hg.hScrollValue, y: hg.vScrollValue};
  },

  getViewingWindow() {
    let hg = this._getHypergrid(),
        [vs, hs] = [hg.vScrollValue, hg.hScrollValue],
        [cols, rows] = [hg.getVisibleColumns(), hg.getVisibleRows()];
        // This might fail on the initial load, since getVisibleColumns() and
        //getVisibleRows() might return nothing, ergo the below hack.
        let colLength = cols.length, rowLength = rows.length;
        if (colLength == 0) colLength = 20;
        if (rowLength == 0) rowLength = 30;
    return { range: {tl: {row: vs+1, col: hs+1},
                     br: {row: vs + rowLength, col: hs + colLength}} };
  },

  isVisible(col, row){ // faster than accessing hypergrid properties
    return (this.state.scroll.x <= col && col <= this.state.scroll.x+Constants.numVisibleCols) &&
           (this.state.scroll.y <= row && row <= this.state.scroll.y+Constants.numVisibleRows);
  },

  getVisibleRows() {
    return this._getHypergrid().getVisibleRows().length;
  },

  getTextboxPosition() {
    let scroll = this.state.scroll;
    if (Store.getActiveSelection()){
      let {col, row} = Store.getActiveSelection().origin,
          point = finRect.point.create(col - scroll.x, row - scroll.y);
      return this._getHypergrid().getBoundsOfCell(point);
    }
    else {
      return null;
    }
  },


  /*************************************************************************************************************************/
  // Hypergrid initialization

  /* Initial a sheet with blank entries */
  initialize() {
    let hg = this._getHypergrid(),
        model = hg.getBehavior(),
        self = this;
    hg.addGlobalProperties(this.gridProperties);
    model.getColumnCount = () => { return Constants.numCols; };
    model.getRowCount = () => { return Constants.numRows; };
    model.getValue = (x, y) => { return ''; };
    model.getCellEditorAt = (x, y) => { return null; };
    model.handleMouseDown = (grid, evt) => {
      if (Store.getGridShifted()) { // shift+click
        let {origin} = this.getSelectionArea(),
            newBr = {col: evt.gridCell.x, row: evt.gridCell.y},
            newSel = {origin: origin, range: Util.orientRange({tl: origin, br: newBr})};
        this.select(newSel, false);
      } else {
        let {x, y} = this.getCoordsFromMouseEvent(grid, evt);
        if (this.insideBox(evt) && !evt.primitiveEvent.detail.isRightClick) {
          // dragging blue box
          this.mouseDownInBox = true;
        } else if (Render.isOnSelectionEdge(x, y)) {
          // dragging selections
          this.dragSelectionOrigin = {col: evt.gridCell.x, row: evt.gridCell.y};
        } else if (model.featureChain) {
          model.featureChain.handleMouseDown(grid, evt);
          model.setCursor(grid);
        }
      }
    };

    model.onMouseMove = (grid, evt) => {
      let {x, y} = this.getCoordsFromMouseEvent(grid, evt);
      if (this.insideBox(evt)) {
        this.setState({cursorStyle:'crosshair'});
      } else if (Render.isOnSelectionEdge(x, y)) {
        this.setState({cursorStyle: 'move'});
      } else {
        self.setState({cursorStyle:'auto'});
        if (model.featureChain) {
          model.featureChain.handleMouseMove(grid, evt);
          model.setCursor(grid);
        }
      }
    };

    model.onMouseDrag = (grid, evt) => {
      if (this.dragSelectionOrigin !== null) {
        // range dragging
        console.log("\n\n\nSELECTION DRAG!!\n\n\n", evt);
        let {x, y} = this.getCoordsFromMouseEvent(grid, evt);
        let {range} = this.getSelectionArea();
        this.drawDraggedSelection(this.dragSelectionOrigin, range, evt.gridCell.x, evt.gridCell.y);
        this.mousePosition = {x:evt.primitiveEvent.detail.mouse.x,
                              y:evt.primitiveEvent.detail.mouse.y};
        this.scrollWithDraggables(grid);
        this.repaint();
      } else if (this.mouseDownInBox && !evt.primitiveEvent.detail.isRightClick) {
        // box dragging
        let {x,y} = evt.gridCell; // accounts for scrolling
        Render.setDragCorner({dragX:x,dragY:y});
        this.mousePosition = {x:evt.primitiveEvent.detail.mouse.x,
                              y:evt.primitiveEvent.detail.mouse.y};
        this.scrollWithDraggables(grid);
        this.repaint(); // show dotted lines
      } else if (model.featureChain) {
        // do default
        model.featureChain.handleMouseDrag(grid, evt);
        model.setCursor(grid);
      }
    };

    model.onMouseUp = (grid, evt) => {
      if (this.dragSelectionOrigin !== null) {
        let {x, y} = this.getCoordsFromMouseEvent(grid, evt);
        let sel = this.getSelectionArea();
        let newSelRange = Render.getDragRect(),
            fromRange = TC.simpleToASRange(sel.range),
            toRange = TC.simpleToASRange(newSelRange),
            newSel = {range: newSelRange, origin: newSelRange.tl};
        this.select(newSel, false);
        Render.setDragRect(null);
        this.dragSelectionOrigin = null;
        API.cut(fromRange, toRange);
      } else if (Render.getDragCorner() !== null) {
        let dottedSel = Render.getDottedSelection();
        // Do nothing if the mouseup isn't in the right column or row
        if (dottedSel.range !== null){
          API.drag(Store.getActiveSelection().range,dottedSel.range);
          self.select(dottedSel,true);
        }
      } else if (model.featureChain) {
        model.featureChain.handleMouseUp(grid, evt);
        model.setCursor(grid);
      }
      Render.setDragCorner(null);
      self.mouseDownInBox = false;
    };

    this.setCellRenderer();
    this.setRenderers();
    let ind = {row: 1, col: 1};
    // This will make the first selection have properties of a click
    // Namely, the blue box will show up
    ExpStore.setClickType(Constants.ClickType.CLICK);
    this.select({origin: ind, range: {tl: ind, br: ind}}, false);
  },

  // expects that the current sheet has already been set
  getInitialData(){
    API.openSheet(Store.getCurrentSheet());
    ActionCreator.scroll(this.getViewingWindow());
  },

  gridProperties: {
    editorActivationKeys: [], // disable column picker
    scrollbarHoverOff: 'visible',
    columnAutosizing: true
  },

  /*************************************************************************************************************************/
  // Hypergrid update display

  /* Called by eval pane's onChange method, when eval pane receives a change event from the store */
  updateCellValues(clientCells) {
    let model = this._getBehavior();
    // update the hypergrid values
    clientCells.forEach((c) => {
      let cellSheetId = c.cellLocation.sheetId,
          gridCol = c.cellLocation.index.col-1, // hypergrid starts indexing at 0
          gridRow = c.cellLocation.index.row-1, // hypergrid starts indexing at 0
          display = Util.showValue(c.cellValue);

      model.setValue(gridCol, gridRow, display.toString());
      let overlay = Util.getOverlay(c.cellValue, gridCol, gridRow);
      if (overlay)
        this.addOverlay(overlay);
    });

    model.changed(); // causes hypergrid to show updated values
    Store.resetLastUpdatedCells();
  },

  // update grid overlays (images, charts, etc)
  addOverlay(overlay) {
    let overlays = this.state.overlays;
    overlays.push(overlay);
    this.setState({overlays: overlays});
  },

  /*************************************************************************************************************************/
  // Hypergrid methods for updating selection, focus, scrolling

  repaint() {
    this._getHypergrid().repaint();
  },

  setFocus() {
    this._getHypergrid().takeFocus();
  },
  // do not call before polymer is ready.
  select(unsafeSelection, shouldScroll) {
    logDebug("Spreadsheet select start");
    if (typeof(shouldScroll) == "undefined") {
      shouldScroll = true;
    }

    // unsafe if it references values <= 0.
    let safeSelection = Util.getSafeSelection(unsafeSelection);
    let {tl, br} = safeSelection.range;
    let {col, row} = safeSelection.origin;

    let oldSel = Store.getActiveSelection();
    // make selection
    let hg = this._getHypergrid(),
        originIsCorner = Util.originIsCornerOfSelection(safeSelection),
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
      this.scrollTo(scroll.scrollH, scroll.scrollV);
    }
    this.repaint();
    this.props.onSelectionChange(safeSelection);
  },

  _getNewScroll(oldSel, newSel) {
    let hg = this._getHypergrid();
    let {tl, br} = newSel.range;
    let {col, row} = newSel.origin;

    let oldOrigin, oldRange, oldTl, oldBr;
    if (oldSel) {
        oldOrigin = oldSel.origin;
        oldRange = oldSel.range;
        oldTl = oldRange.tl;
        oldBr = oldRange.br;
    }

    let win = this.getViewingWindow().range;
    let scrollH = hg.getHScrollValue(), scrollV = hg.getVScrollValue();

    // I think this code is a little hacky; I haven't thought this through deeply to ensure that
    // it works in all cases. It does work for ctrl shift arrows and ctrl arrows though. (Alex 11/3/15)
    if (oldOrigin && oldOrigin.col == col && oldOrigin.row == row) {
      if (win.tl.row <= oldTl.row && oldTl.row <= win.br.row
          && (tl.row < win.tl.row || tl.row > win.br.row)) {
        // if the top left was in range before, and now isn't, scroll so that top left is at top now.
        scrollV = tl.row - 1;
      } else if (win.tl.row <= oldBr.row && oldBr.row <= win.br.row
                 && (br.row < win.tl.row || br.row > win.br.row)) {
        // ditto for bottom right
        scrollV = hg.getVScrollValue() + br.row - win.br.row + 2; // for some reason it works better with the + 2
      }

      if (win.tl.col <= oldTl.col && oldTl.col <= win.br.col
          && (tl.col < win.tl.col || tl.col > win.br.col)) {
        scrollH = tl.col - 1;
      } else if (win.tl.col <= oldBr.col && oldBr.col <= win.br.col
                 && (br.col < win.tl.col || br.col > win.br.col)) {
        scrollH = hg.getHScrollValue() + br.col - win.br.col;
      }
    } else if (oldOrigin) {
      if (col < win.tl.col) {
        scrollH = col - 1;
      } else if (col > win.br.col) {
        scrollH = hg.getHScrollValue() + col - win.br.col;
      }

      if (row < win.tl.row) {
        scrollV = row - 1;
      } else if (row > win.br.row) {
        scrollV = hg.getVScrollValue() + row - win.br.row  + 2;
      }
    }

    return {scrollH: scrollH, scrollV: scrollV};
  },

  shiftSelectionArea(dc, dr) {
    let sel = Store.getActiveSelection();
    let origin = {row: sel.origin.row + dr, col: sel.origin.col + dc};
    let range = {tl: origin, br: origin};
    this.select({range: range, origin: origin});
  },

  scrollTo(x, y){
    let hg = this._getHypergrid();
    if (hg.getHScrollValue() != x || hg.getVScrollValue() != y) {
      hg.setVScrollValue(y),
      hg.setHScrollValue(x);
      ActionCreator.scroll(this.getViewingWindow());
    }
  },

  /*************************************************************************************************************************/
  // Handling events

  _onKeyDown(e){
    logDebug("GRID KEYDOWN", e);
    e.persist(); // prevent react gc
    if (ShortcutUtils.gridShouldDeferKey(e)){ // not a nav key
      KeyUtils.killEvent(e);
      let userIsTyping = ExpStore.getUserIsTyping(),
          clickType    = ExpStore.getClickType();
      logDebug("CLICK TYPE " + clickType);
      if ((KeyUtils.producesTextChange(e) && !KeyUtils.isEvalKey(e) && !KeyUtils.isDestructiveKey(e)) ||
          (KeyUtils.isDestructiveKey(e) && userIsTyping)) {
        // Need to update the editor and textbox now via action creators
        logDebug("Grid key down going to AC");
        let newStr = KeyUtils.modifyTextboxForKey(e,
                                                  userIsTyping,
                                                  clickType,
                                                  ExpStore.getExpression(),
                                                  this.refs.textbox.editor);

        // if visible key and there was a last cell ref, move the selection back to the origin
        if (ExpStore.getLastRef() !== null) {
          this.select(Store.getActiveSelection());
        }
        this.props.hideToast();
        ExpActionCreator.handleGridChange(newStr);
      } else if (KeyUtils.isPureShiftKey(e)) { // shift+click tracking
        Store.setGridShifted(true);
      } else {
        // Try shortcuts
        logDebug("Grid key down, trying shortcut");
        ShortcutUtils.tryShortcut(e, 'common');
        ShortcutUtils.tryShortcut(e, 'grid');
      }
    } else { // nav key from grid
      let {range, origin} = Store.getActiveSelection();
      logDebug("ACTIVE SEL AFTER NAV KEY", origin);
      if (KeyUtils.isPureArrowKey(e) && !T.isIndex(range)) {
        logDebug("MANUALLY HANDLING NAV KEY");
        KeyUtils.killEvent(e);
        let newOrigin = KeyUtils.shiftIndexByKey(e, origin);
        this.select({range: {tl: newOrigin, br: newOrigin}, origin: newOrigin});
      }
      this.props.onNavKeyDown(e);
    }
  },

  _onKeyUp(e) {
    e.persist();
    if (KeyUtils.isPureShiftKey(e)) {
      Store.setGridShifted(false);
      logDebug("\n\nGRID SHIFT KEYUP\n\n", e);
    }
  },

  onTextBoxDeferredKey(e){
    if (e.ctrlKey) { // only for ctrl+arrowkeys
      ShortcutUtils.tryShortcut('grid');
    }
    else {
      this.navByKey(e); // all others handled by grid
    }
  },

  _onFocus(e) {
    /*
    Only sometimes, for reasons I don't fully understand
    (might have something to do with position props) updateTextBox causes onFocus to fire in grid
    If this happens, manually put the focus back in the textbox
    -- RITESH
    */
    logDebug("Grid on focus");
    if (ExpStore.getClickType() === Constants.ClickType.DOUBLE_CLICK){
      Store.setFocus('textbox');
      this.props.setFocus('textbox');
    } else {
      Store.setFocus('grid');
    }
  },

  /*************************************************************************************************************************/
  // Respond to change event from ExpStore

  _onExpressionChange(){
    let xpChangeOrigin = ExpStore.getXpChangeOrigin(),
        xpStr = ExpStore.getExpression();
    logDebug("Grid caught exp update of type: " +  xpChangeOrigin);
    switch(xpChangeOrigin){
      case Constants.ActionTypes.TEXTBOX_CHANGED:
        Render.setShouldRenderSquareBox(false);
        // no square box while typing
        break;
      case Constants.ActionTypes.EDITOR_CHANGED:
      case Constants.ActionTypes.GRID_KEY_PRESSED:
        Render.setShouldRenderSquareBox(false);
        this.repaint();
        this.refs.textbox.updateTextBox(xpStr);
        break;
      case Constants.ActionTypes.NORMAL_SEL_CHANGED:
        this.refs.textbox.hideTextBox(xpStr);
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
      default:
        break;
    }
  },


  /*************************************************************************************************************************/
  // Renderers

  setCellRenderer() {
    let model = this._getBehavior(),
        cellProvider = model.getCellProvider(),
        self = this;
    cellProvider.getCell = function(config) {
      let renderer = Render.defaultCellRenderer,
          col = config.x + 1,
          row = config.y + 1,
          cell = Store.getCell(col, row);

      // tag-based cell styling
      if (cell) {
        config = Util.valueToRenderConfig(config, cell.cellValue);
        if (cell.cellTags.length > 0){
          config = Util.tagsToRenderConfig(config, cell.cellTags);
        }
      } else {
        config.halign = 'center';
      }

      renderer.config = config;
      return renderer;
    }
  },

  setRenderers() {
    let renderer = this._getHypergrid().getRenderer();
    renderer.addExtraRenderer(Render.selectionRenderer);
    renderer.addExtraRenderer(Render.dependencyRenderer);
    renderer.addExtraRenderer(Render.draggingRenderer);
    renderer.addExtraRenderer(Render.cornerBoxRenderer);
    renderer.startAnimator();
  },

  /*************************************************************************************************************************/
  // Render

  render() {
    let {behavior, width, height, language} = this.props; //should also have onReady
    let style = {width: width, height: height,cursor:this.state.cursorStyle};
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
      <div style={{width:"100%",
                   height:"calc(100% - 50px)",
                   position:'relative',
                   cursor: this.state.cursorStyle}} >
        <fin-hypergrid
          style={style}
          ref="hypergrid"
          onKeyDown={this._onKeyDown}
          onKeyUp={this._onKeyUp}
          onFocus={this._onFocus}>
            {behaviorElement}
        </fin-hypergrid>

        {this.state.overlays.map(function (overlay) {
          return (<ASOverlay key={overlay.id}
                             overlay={overlay}
                             scroll={self.state.scroll}
                             onOverlayClick={self.onOverlayClick}
                             isVisible={self.isVisible}/>);
        })}

        <ASRightClickMenu ref="rightClickMenu" />

        <Textbox
                 ref="textbox"
                 language={language.Editor}
                 scroll={self.state.scroll}
                 onDeferredKey={this.props.onTextBoxDeferredKey}
                 hideToast={this.props.hideToast}
                 position={this.getTextboxPosition}/>

      </div>
    );
  }

});
