import React from 'react';
import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import ShortcutUtils from '../AS/ShortcutUtils';
import Converter from '../AS/Converter';
import API from '../actions/ASApiActionCreators';
import KeyUtils from '../AS/KeyUtils';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';
import Constants from '../Constants';
import Render from '../AS/Render';

import ASOverlay from './ASOverlay.jsx';
import Textbox from './Textbox.jsx'

export default React.createClass({

  /*************************************************************************************************************************/
  /* Default getter methods */

  _getHypergrid() {
    return React.findDOMNode(this.refs.hypergrid);
  },
  _getBehavior(){
    return this._getHypergrid().getBehavior();
  },
  getDefaultProps() {
    return {
      behavior: 'default',
      onReady() { }
    };
  },
  getInitialState() {
    return {
      scroll: { // keep scroll values in state so overlays autoscroll with grid
        x:0,
        y:0
      },
      overlays: []
    };
  },
  // produces oriented area -- row < row2 and col < col2 always
  getSelectionArea() {
    let hg = this._getHypergrid();
    let selection = hg.getSelectionModel().selections[0];
    let ul = selection.origin;
    let range = Util.getOrientedArea({
                  row:  ul.y + 1,
                  col:  ul.x + 1,
                  row2: ul.y + selection.height() + 1,
                  col2: ul.x + selection.width() + 1
                });
    let area = {
        width:  range.col2 - range.col + 1,
        height: range.row2 - range.row + 1,
      };

    if (range.row === range.row2 && range.col === range.col2)
      area.range = {row: range.row, col: range.col};
    else
      area.range = range;
    return area;
  },
  /* Returns the position of scroll */
  getScroll() {
    let hg = this._getHypergrid();
    return {x: hg.hScrollValue, y: hg.vScrollValue};
  },

  // do not call before polymer is ready.
  makeSelection(loc) {
    console.log("making selection!");
    let hg = this._getHypergrid();
    let c = loc.col - 1,
        r = loc.row - 1,
        dC = loc.row2 ? loc.col2 - loc.col : 0,
        dR = loc.col2 ? loc.row2 - loc.row : 0;
    hg.select(c, r, dC, dR);
    this.repaint();
    this.props.onSelectionChange({range:loc, width:dC+1, height:dR+1});
  },
  getViewingWindow() {
    let hg = this._getHypergrid();
    let [vs, hs] = [hg.vScrollValue, hg.hScrollValue];
    let [cols, rows] = [hg.getVisibleColumns(), hg.getVisibleRows()];
    return { range: {row: vs+1, col: hs+1, row2: vs + rows.length, col2: hs + cols.length}, width: cols.length, height: rows.length };
  },
  getViewingWindowWithCache() {
    let vwindow = this.getViewingWindow(), rng = vwindow.range;
    rng = {row: rng.row - Constants.scrollCacheY, col: rng.col - Constants.scrollCacheY,
            row2: rng.row2 + Constants.scrollCacheY, col2: rng.col2 + Constants.scrollCacheX};
    return {range: Converter.getSafeLoc(rng)};
  },
  isVisible(col, row){ // faster than accessing hypergrid properties
    return (this.state.scroll.x <= col && col <= this.state.scroll.x+Constants.numVisibleCols) &&
           (this.state.scroll.y <= row && row <= this.state.scroll.y+Constants.numVisibleRows);
  },
  /*************************************************************************************************************************/
  // Display values in spreadsheet

  /* Initial a sheet with blank entries */
  initialize(){
    let model = this._getBehavior();
    model.getValue = function(x, y) {
      return '';
    };
    this.makeSelection({row: 1, col: 1});
  },
  getInitialData(){
    // expects that the current sheet has already been set
    // e,g, by open/new dialog
    API.sendOpenMessage(Store.getCurrentSheet());
    API.updateViewingWindow({range: {row: 1, col: 1,
        row2: Constants.numVisibleRows + Constants.scrollCacheY,
        col2: Constants.numVisibleCols + Constants.scrollCacheX}});
  },
  /* Called by eval pane's onChange method, when eval pane receives a change event from the store */
  updateCellValues(clientCells){
    // console.log("About to display cells in sheet: " + JSON.stringify(clientCells));
    let model = this._getBehavior();
    for (var key in clientCells){ // update the hypergrid values
      let c = clientCells[key];
      let gridCol = Converter.clientCellGetCol(c)-1; // hypergrid starts indexing at 0
      let gridRow = Converter.clientCellGetRow(c)-1; // hypergrid starts indexing at 0
      let display = Converter.clientCellGetDisplay(c);
      // console.log("Updating display value: " + gridCol + " " + gridRow + " " + display);
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
  // Handling events

  handleKeyDown(e) {
    e.persist(); // prevent react gc
    if (ShortcutUtils.gridShouldDeferKey(e)){ // if anything but nav keys, bubble event to parent
      KeyUtils.killEvent(e);
      this.props.onDeferredKey(e);
    } else if (ShortcutUtils.textBoxShouldDeferKey(e)){
      KeyUtils.killEvent(e);
      this.props.onTextBoxDeferredKey(e);
    }
  },

  onOverlayClick(col, row) {
    // console.log("overlay clicked!");
    // this.makeSelection({col: col, row: row});
  },

  /*************************************************************************************************************************/
  // React methods & grid initialization

  gridProperties: {
    editorActivationKeys: [] // disable column picker
  },

  componentDidMount() {
    document.addEventListener('polymer-ready', () => {
      this.props.onReady();
      this.initialize();
      this.setCellRenderer();
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
          //let { range, width, height } = self.getSelectionArea();
          self.props.onSelectionChange(self.getSelectionArea());
          },
        'fin-scroll-x': function (event) {
          // let {x, y} = self.getScroll();
          self.setState({scroll: self.getScroll()});
          if ((self.getScroll()).x % 20 === 0)
            ActionCreator.scroll(self.getViewingWindowWithCache());
          },
        'fin-scroll-y': function (event) {
          // let {x, y} = self.getScroll();
          self.setState({scroll: self.getScroll()});
          if ((self.getScroll()).y % 20 === 0)
            ActionCreator.scroll(self.getViewingWindowWithCache());
          }
      });
      for (var key in callbacks) {
        var value = callbacks[key];
        hg.addFinEventListener(key, value);
      }
    });
  },

  render() {
    if (this.state.textBox)
      console.log("rendering spreadsheet " + this.state.textBox.xp);
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
   
    // Put overlays with high z Index outside hypergrid
    return (
      <div style={{width:"100%",height:"100%",position:'relative'}} >
        <fin-hypergrid
          style={style}
          ref="hypergrid"
          onKeyDown={this.handleKeyDown}>
            {behaviorElement}
        </fin-hypergrid>

        {this.state.overlays.map(function (overlay) {
          return (<ASOverlay key={overlay.id}
                             overlay={overlay}
                             scroll={self.state.scroll}
                             onOverlayClick={self.onOverlayClick}
                             textBoxChange={this.props.textBoxChange}
                             isVisible={self.isVisible}/>);
        })}

      
        <Textbox ref="textbox" 
                 scroll={self.state.scroll}
                 onKeyDown={this.handleKeyDown}
                 textBoxChange={this.props.textBoxChange}/> 
    
      </div>
    );
  },

  /*************************************************************************************************************************/
  // Hypergrid methods

  repaint() {
    this._getHypergrid().repaint();
  },

  /*************************************************************************************************************************/
  // Rendering

  setCellRenderer() {
    let model = this._getBehavior(),
        cellProvider = model.getCellProvider(),
        self = this;
    cellProvider.getCell = function(config) {
      let renderer = Render.defaultCellRenderer,
          col = config.x + 1,
          row = config.y + 1,
          cell = Store.getCellAtLoc(col, row),
          sel = Store.getActiveSelection().range,
          activeCell = Store.getActiveCell(),
          clipboard = Store.getClipboard();

      // initialize custom config attributes for every cell
      // (hypergrid reuses the config object for performance; attributes must be set explicitly null)
      config.paintBorders = [];

      // console.log(config.value)
      if (cell && activeCell) {
        // console.log("rendering non null cells");
      }
      // tag-based cell styling
      if (cell) {
        if (cell.cellTags.length > 0){
          for (var i=0; i<cell.cellTags.length; i++)
            config = Util.parseTagIntoRenderConfig(config, cell.cellTags[i]);
        }
      } else {
        // default renderer
        config.halign = 'center';
      }

      // selection dependency highlighting
      if (sel && activeCell && activeCell.cellExpression.dependencies) {
        let locs = activeCell.cellExpression.dependencies;
        // console.log("highlighting dependency: "+JSON.stringify(activeCell));
        if (Util.isContainedInLocs(col, row, locs)){
          config.paintBorders = Util.getPaintedBorders(col, row, locs);
          // console.log("drawing borders: " + JSON.stringify(config.paintBorders));
          config.bgColor = "#d3d3d3"; // light grey fill
          config.borderConfig = {lineType: 0, // solid border type
                                 width: 2,
                                 color: "#000000"}; // black border color
        }
      }

      // Cell/Range highlighting
      if (sel) {
        if (Util.isContainedInLocs(col, row, sel)) {
          config.paintBorders = Util.getPaintedBorders(col, row, sel);
          config.borderConfig = {lineType: 0, // solid border type
                                width: 3,
                                color: "#000000"}; // black border color
        }
      }



      // clipboard highlighting
      if (clipboard.range && Util.isContainedInLocs(col, row, clipboard.range)) {
        config.paintBorders = Util.getPaintedBorders(col, row, clipboard.range);
        config.borderConfig = {lineType: 1, // dashed border type
                                 width: 3,
                                 color: clipboard.isCut ? "#ff0000" : "#4169e1"}; // red cut, blue copy
      }

      renderer.config = config;
      return renderer;
    }
  }
});
