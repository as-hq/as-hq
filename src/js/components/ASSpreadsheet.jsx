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
import Util from '../AS/Util';
import Constants from '../Constants';
import Render from '../AS/Render';

import ASOverlay from './ASOverlay.jsx';
import Textbox from './Textbox.jsx'


export default React.createClass({

  /*************************************************************************************************************************/
  // React methods

  propTypes: {
    onSelectionChange: React.PropTypes.func.isRequired,
    onTextBoxDeferredKey: React.PropTypes.func.isRequired,
    onNavKeyDown: React.PropTypes.func.isRequired
  },

  // TODO: do we actually need behavior??
  getDefaultProps() {
    return {
      behavior: 'default',
      onReady() { }
    };
  },

  getInitialState() {
    return {
      // keep scroll values in state so overlays autoscroll with grid
      scroll: { x:0, y:0 },
      overlays: []
    };
  },

  componentDidMount() {
    // Be able to respond to events from ExpStore
    ExpStore.addChangeListener(this._onExpressionChange);
    // Hypergrid initialization
    document.addEventListener('polymer-ready', () => {
      this.props.onReady();
      this.initialize();
      this.setCellRenderer();
      // this.setSelectionRenderer();
      let self = this;
      let hg = this._getHypergrid();
      hg.addGlobalProperties(this.gridProperties);
      this.getInitialData();
      let callbacks = ({
        /*
          Call onSelectionChange method in eval pane to deal with selection change
          Need to also figure out the expression to render in the editor
        */
        'fin-selection-changed': function (event) {
          console.log("SELECTION CHANGE");
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
          console.log("DOUBLE ClICK");
          self.refs.textbox.updateTextBox(ExpStore.getExpression());
          }
      });
      for (var key in callbacks) {
        var value = callbacks[key];
        hg.addFinEventListener(key, value);
      }
    });
  },

  componentWillUnmount(){
    ExpStore.removeChangeListener(this._onExpressionChange);
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
          width:  range.br.col - range.tl.col + 1,
          height: range.br.row - range.tl.row + 1,
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
    return { range: {tl: {row: vs+1, col: hs+1},
                     br: {row: vs + rows.length, col: hs + cols.length}},
             width: cols.length,
             height: rows.length };
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
          rect = document.createElement('fin-rectangle'),
          point = rect.point.create(col - scroll.x, row - scroll.y);
      return this._getHypergrid().getBoundsOfCell(point);
    }
    else {
      return null;
    }
  },


  /*************************************************************************************************************************/
  // Hypergrid initialization

  /* Initial a sheet with blank entries */
  initialize(){
    let model = this._getBehavior();
    model.getValue = function(x, y) { return ''; };
    model.getCellEditorAt = function(x, y) { return null; }
    this.select({tl: {row: 1, col: 1}, br: {row: 1, col: 1}}, {row: 1, col: 1}, false);
  },
  // expects that the current sheet has already been set
  getInitialData(){
    API.openSheet(Store.getCurrentSheet());
    ActionCreator.scroll(this.getViewingWindow());
  },

  gridProperties: {
    editorActivationKeys: [] // disable column picker
  },

  /*************************************************************************************************************************/
  // Hypergrid update display

  /* Called by eval pane's onChange method, when eval pane receives a change event from the store */
  updateCellValues(clientCells){
    let model = this._getBehavior();
    for (var key in clientCells){ // update the hypergrid values
      let c = clientCells[key],
          gridCol = c.cellLocation.index.col-1, // hypergrid starts indexing at 0
          gridRow = c.cellLocation.index.row-1, // hypergrid starts indexing at 0
          display = Util.showValue(c.cellValue);

      model.setValue(gridCol,gridRow,display.toString());
      let overlay = Util.getOverlay(c.cellValue, gridCol, gridRow);
      if (overlay)
        this.addOverlay(overlay);
    }
    model.changed(); // causes hypergrid to show updated values
    Store.resetLastUpdatedCells();
  },

  // update grid overlays (images, charts, etc)
  addOverlay(overlay) {
    console.log("added overlay!");
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
  select(unsafeLoc, origin, shouldScroll) {
    let loc = Util.getSafeRange(unsafeLoc),
        {tl, br} = loc;
    console.log("making selection!", loc);

    // make selection
    let hg = this._getHypergrid(),
        c = tl.col - 1,
        r = tl.row - 1,
        dC = br.col - tl.col,
        dR = br.row - tl.row;
    hg.takeFocus();
    hg.clearSelections();
    hg.select(c, r, dC, dR);

    // set mousedown
    // hypergrid sucks -- doesn't set the mouse focus automatically
    // with select, so we have to do it ourselves.
    let finRect = document.createElement('fin-rectangle'),
        myDown = finRect.point.create(c,r),
        myExtent = finRect.point.create(dC, dR);
    hg.setMouseDown(myDown);
    hg.setDragExtent(myExtent);

    // set scroll
    let range = this.getViewingWindow().range,
        shouldScrollH = tl.col < range.tl.col || tl.col > range.br.col,
        shouldScrollV = tl.row < range.tl.row || tl.row > range.br.row,
        scrollH = shouldScrollH ? c : hg.getHScrollValue(),
        scrollV = shouldScrollV ? r : hg.getVScrollValue();
    if ((shouldScrollV || shouldScrollH) && shouldScroll) {
      this.scrollTo(scrollH, scrollV);
    }

    this.repaint();
    this.props.onSelectionChange({range:loc,
                                  width:dC+1,
                                  height:dR+1,
                                  origin: origin});
  },

  shiftSelectionArea(dc, dr){
    console.log(dc, dr);
    let sel = Store.getActiveSelection(),
        {tl, br} = sel.range,
        range = {tl: {row: tl.row + dr, col: tl.col + dc},
                 br: {row: br.row + dr, col: br.col + dc} },
        origin = {row: sel.origin.row + dr, col: sel.origin.col + dc};
    this.select(Util.getSafeRange(range), Util.getSafeIndex(origin), true);
  },

  navByKey(e) {
    // TODO
    console.log("navbykey called");
  },

  scrollTo(x, y){
    let hg = this._getHypergrid();
    hg.setVScrollValue(y),
    hg.setHScrollValue(x);
    ActionCreator.scroll(this.getViewingWindow());
  },

  /*************************************************************************************************************************/
  // Handling events

  _onKeyDown(e){
    console.log("\n\nGRID KEYDOWN");
    e.persist(); // prevent react gc
    if (ShortcutUtils.gridShouldDeferKey(e)){ // not a nav key
      KeyUtils.killEvent(e);
      if (KeyUtils.producesVisibleChar(e) && e.which !== 13) {
        // Need to update the editor and textbox now via action creators
        console.log("Grid key down going to AC");
        let curStr = ExpStore.getExpression(),
            newStr = KeyUtils.modifyStringForKey(curStr, e);
        // ^ modify string for key deals with ctrl+backspace too
        // if visible key and there was a last cell ref, move the selection back to the origin
        if (ExpStore.getLastRef() !== null) {
          let {range, origin} = Store.getActiveSelection();
          this.select(range, origin);
        }
        ExpActionCreator.handleGridChange(newStr);
      } else {
        // Try shortcuts
        console.log("Grid key down, trying shortcut");
        ShortcutUtils.tryShortcut(e, 'common');
        ShortcutUtils.tryShortcut(e, 'grid');
      }
    } else { // nav key from grid
      let {range, origin} = Store.getActiveSelection();
      if (KeyUtils.isPureArrowKey(e) && !T.isIndex(range)) {
        KeyUtils.killEvent(e);
        let newOrigin = KeyUtils.shiftIndexByKey(e, origin);
        this.select({tl: newOrigin, br: newOrigin}, newOrigin, true);
      }
      this.props.onNavKeyDown(e);
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
    Store.setFocus('grid');
  },

  /*************************************************************************************************************************/
  // Respond to change event from ExpStore

  _onExpressionChange(){
    let xpChangeOrigin = ExpStore.getXpChangeOrigin(),
        xpStr = ExpStore.getExpression();
    switch(xpChangeOrigin){
      case Constants.ActionTypes.EDITOR_CHANGED:
        console.log("Grid caught exp update of EDITOR type");
        this.refs.textbox.updateTextBox(xpStr);
        break;
      case Constants.ActionTypes.GRID_KEY_PRESSED:
        console.log("Grid caught exp update of GRID type");
        this.refs.textbox.updateTextBox(xpStr);
        break;
      case Constants.ActionTypes.NORMAL_SEL_CHANGED:
        console.log("Grid caught exp update of SEL_CHNG type");
        this.refs.textbox.hideTextBox(xpStr);
        break;
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_GRID:
        console.log("Grid caught PARTIAL GRID");
        this.refs.textbox.updateTextBox(xpStr);
        break;
      case Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_EDITOR:
        console.log("Grid caught PARTIAL EDITOR");
        this.refs.textbox.updateTextBox(xpStr);
        break;
      case Constants.ActionTypes.ESC_PRESSED:
        console.log("Grid caught ESC");
        this.refs.textbox.updateTextBox(xpStr);
        this.refs.textbox.hideTextBox();
        break;
      default:
        // don't need to do anything on TEXTBOX_CHANGED
        // or PARTIAL_REF_CHANGE_WITH_TEXTBOX
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
          cell = Store.getCell(col, row),
          sel = Store.getActiveSelection(),
          rng = sel.range,
          activeCell = Store.getActiveCell(),
          clipboard = Store.getClipboard();

      // initialize custom config attributes for every cell
      // (hypergrid reuses the config object for performance; attributes must be set explicitly null)
      config.paintBorders = [];
      config.halign = 'left';

      // tag-based cell styling
      if (cell) {
        config = Util.valueToRenderConfig(config, cell.cellValue);
        if (cell.cellTags.length > 0){
          config = Util.tagsToRenderConfig(config, cell.cellTags);
        }
      } else {
        config.halign = 'center';
      }

      // selection dependency highlighting
      if (rng && activeCell && activeCell.cellExpression.dependencies) {
        let locs = activeCell.cellExpression.dependencies;
        if (Util.isContainedInLocs(col, row, locs)){
          config.paintBorders = Util.getPaintedBorders(col, row, locs);
          config.bgColor = "#d3d3d3"; // light grey fill
          config.borderConfig = {lineType: 0, // solid border type
                                 width: 2,
                                 color: "#000000"}; // black border color
        }
      }

      // Cell/Range highlighting
      if (rng) {
        if (Util.isContainedInLocs(col, row, [rng])) {
          config.paintBorders = Util.getPaintedBorders(col, row, [rng]);
          config.borderConfig = {lineType: 0, // solid border type
                                width: 2,
                                color: "#003EFF"}; // blue border color
        }
      }

      // origin border
      if (sel.origin.col === col && sel.origin.row === row) {
        config.paintBorders = Util.getPaintedBordersForSingleCell();
        config.borderConfig = {lineType: 0, // solid border type
                                 width: 3,
                                 color: "#003EFF"}; // blue border color
      }

      // clipboard highlighting
      if (clipboard.area && Util.isContainedInLocs(col, row, [clipboard.area.range])) {
        config.paintBorders = Util.getPaintedBorders(col, row, [clipboard.area.range]);
        config.borderConfig = {lineType: 1, // dashed border type
                                 width: 3,
                                 color: clipboard.isCut ? "#ff0000" : "#4169e1"}; // red cut, blue copy
      }

      renderer.config = config;
      return renderer;
    }
  },

  // setSelectionRenderer() {
  //   let hg = this._getHypergrid(),
  //       renderer = hg.getRenderer();
  //   var img =  new Image;
  //   var imageX = 3;
  //   var imageY = 45;
  //   img.src = 'http://img3.wikia.nocookie.net/__cb20130601171117/degrassi/images/5/5c/Wanted-bunny-rabbit_50154511.jpg';
  //   renderer.addExtraRenderer(Render.selectionRenderer);
  //   renderer.startAnimator();
  // },

  /*************************************************************************************************************************/
  // Render

  render() {
    let {behavior, width, height} = this.props; //should also have onReady
    let style = {width: width, height: height};
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
      <div style={{width:"100%",height:"100%",position:'relative'}} >
        <fin-hypergrid
          style={style}
          ref="hypergrid"
          onKeyDown={this._onKeyDown}>
            {behaviorElement}
          onFocus={this._onFocus}
        </fin-hypergrid>

        {this.state.overlays.map(function (overlay) {
          return (<ASOverlay key={overlay.id}
                             overlay={overlay}
                             scroll={self.state.scroll}
                             onOverlayClick={self.onOverlayClick}
                             isVisible={self.isVisible}/>);
        })}


        <Textbox
                 ref="textbox"
                 scroll={self.state.scroll}
                 onDeferredKey={this.props.onTextBoxDeferredKey}
                 position={this.getTextboxPosition}/>

      </div>
    );
  }

});
