import React from 'react';
import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import Shortcuts from '../AS/Shortcuts';
import Converter from '../AS/Converter';
import API from '../actions/ASApiActionCreators';
import KeyUtils from '../AS/KeyUtils';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';
import Render from '../AS/Render';

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
  /* TODO */
  makeSelection(loc) {
  },
  getViewingWindow() {
    let hg = this._getHypergrid();
    let [vs, hs] = [hg.vScrollValue, hg.hScrollValue];
    let [cols, rows] = [hg.getVisibleColumns(), hg.getVisibleRows()];
    return { range: {row: vs, col: hs, row2: vs + cols.length - 1, col2: hs + rows.length - 1}, width: cols.length, height: rows.length };
  },

  /*************************************************************************************************************************/
  // Display values in spreadsheet

  /* Initial a sheet with blank entries */
  initializeBlank(){
    let model = this._getBehavior();
    model.getValue = function(x, y) {
      return '';
    };
  },
  // TODO
  getInitialData(){

  },
  /* Called by eval pane's onChange method, when eval pane receives a change event from the store */
  updateCellValues(clientCells){
    console.log("About to display cells in sheet: " + JSON.stringify(clientCells));
    let model = this._getBehavior();
    for (var key in clientCells){ // update the hypergrid values
      let c = clientCells[key];
      let gridCol = Converter.clientCellGetCol(c)-1; // hypergrid starts indexing at 0
      let gridRow = Converter.clientCellGetRow(c)-1; // hypergrid starts indexing at 0
      let display = Converter.clientCellGetDisplay(c);
      console.log("Updating display value: " + gridCol + " " + gridRow + " " + display);
      model.setValue(gridCol,gridRow,display);
    }
    model.changed(); // causes hypergrid to show updated values
  },

  /*************************************************************************************************************************/
  // Handling keyboard shortcuts

  handleKeyDown(e) {
    e.persist(); // prevent react gc
    if (Shortcuts.gridShouldDeferKey(e)){ // if anything but nav keys, bubble event to parent
      KeyUtils.killEvent(e);
      this.props.onDeferredKey(e);
    } else {
      console.log("native grid event allowed");
    }
  },

  /*************************************************************************************************************************/
  // React methods & grid initialization

  gridProperties: {
    editorActivationKeys: [] // disable column picker
  },

  componentDidMount() {
    document.addEventListener('polymer-ready', () => {
      this.props.onReady();
      this.initializeBlank();
      this.setCellRenderer();
      let self = this;
      let hg = this._getHypergrid();
      hg.addGlobalProperties(this.gridProperties);
      let callbacks = ({
        /*
          Call onSelectionChange method in eval pane to deal with selection change
          Need to also figure out the expression to render in the editor
        */
        'fin-selection-changed': function (event) {
          let { range, width, height } = self.getSelectionArea();
          self.props.onSelectionChange(range);
          },
        'fin-scroll-x': function (event) {
          // let {x, y} = self.getScroll();
          ActionCreator.scroll(self.getViewingWindow());
          },
        'fin-scroll-y': function (event) {
          // let {x, y} = self.getScroll();
          ActionCreator.scroll(self.getViewingWindow());
          }
      });
      for (var key in callbacks) {
        var value = callbacks[key];
        hg.addFinEventListener(key, value);
      }
    });
  },

  render() {
    let {behavior, width, height} = this.props; //should also have onReady
    let style = {width: width, height: height};
    let behaviorElement;
    switch (behavior) {
      case 'json':
        behaviorElement = <fin-hypergrid-behavior-json />;
        break;
      case 'default':
        behaviorElement = <fin-hypergrid-behavior-default />
        break;
    }

    return (
      <fin-hypergrid
        style={style}
        ref="hypergrid"
        onKeyDown={this.handleKeyDown}>
          {behaviorElement}
      </fin-hypergrid>
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
          sel = Store.getActiveSelection(),
          activeCell = Store.getActiveCell(),
          clipboard = Store.getClipboard();

      // initialize custom config attributes for every cell
      // (hypergrid reuses the config object for performance; attributes must be set explicitly null)
      config.paintBorders = [];

      // tag-based cell styling
      if (cell.cellTags.length > 0) {
        for (var i=0; i<cell.cellTags.length; i++)
          config = Util.parseTagIntoRenderConfig(config, cell.cellTags[i]);
      } else {
        // default renderer
        config.halign = 'center';
      }

      // selection dependency highlighting
      if (sel && activeCell) {
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

      // range highlighting
      if (sel && sel.row2) {
        if (Util.isContainedInLocs(col, row, sel)) {
          config.paintBorders = Util.getPaintedBorders(col, row, sel);
          config.borderConfig = {lineType: 0, // solid border type
                                width: 3,
                                color: "#4169e1"}; // blue
        }
      }

      // clipboard highlighting
      if (clipboard.range && Util.isContainedInLocs(col, row, clipboard.range)) {
        config.paintBorders = Util.getPaintedBorders(col, row, clipboard.range);
        config.borderConfig = {lineType: 1, // dashed border type
                                 width: 3,
                                 color: clipboard.isCut ? "#ff0000" : "#4169e1"}; // red cut, blue copy
      }

      // image rendering
      if (cell.cellValue.tag === "ValueImage"){
        // console.log("setting image!");
        renderer = Render.imageCellRenderer;
        let img = document.createElement('img');
        img.src = cell.cellValue.imagePath;
        img.alt = "Error rendering image";
        img.width = 100;
        img.height = 100;
        config.ASImage = img;
      }
      renderer.config = config;
      return renderer;
    }
  }
});
