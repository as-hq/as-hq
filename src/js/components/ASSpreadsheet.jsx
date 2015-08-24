import React from 'react';
import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import Shortcuts from '../AS/Shortcuts';
import Converter from '../AS/Converter';
import API from '../actions/ASApiActionCreators';
import KeyUtils from '../AS/KeyUtils';
import Store from '../stores/ASEvaluationStore';
import Util from '../AS/Util';

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
  getSelectionArea() {
    let hg = this._getHypergrid();
    let selection = hg.getSelectionModel().selections[0];
    let ul = selection.origin;
    let range = {
                  row:  ul.y + 1,
                  col:  ul.x + 1,
                  row2: ul.y + selection.height() + 1,
                  col2: ul.x + selection.width() + 1
                };
    if (range.row === range.row2 && range.col === range.col2)
      return {
        width:  selection.width() + 1,
        height: selection.height() + 1,
        range: {row: range.row, col: range.col}
      };
    else return {
        width: selection.width() + 1,
        height: selection.height() + 1,
        range:range
    };
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
  // React methods

  componentDidMount() {
    document.addEventListener('polymer-ready', () => {
      this.props.onReady();
      this.initializeBlank();
      this.setCellRenderer();
      let self = this;
      let hg = this._getHypergrid();
      let callbacks = ({
        /*
          Call onSelectionChange method in eval pane to deal with selection change
          Need to also figure out the expression to render in the editor
        */
        'fin-selection-changed': function (event) {
          let { range, width, height } = self.getSelectionArea();
          if (width === 1 && height === 1)
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
  // Rendering

  renderCellBorder(config) {
    // TODO
    let renderer = this._getHypergrid().getRenderer();
  },

  renderImage(fpath) {
    // TODO
  },

  bordered: {
    paint: function(gc, x, y, width, height, isLink) {
      isLink = isLink || false;
      var colHEdgeOffset = this.config.properties.cellPadding,
          halignOffset = 0,
          valignOffset = this.config.voffset,
          halign = this.config.halign,
          isColumnHovered = this.config.isColumnHovered,
          isRowHovered = this.config.isRowHovered,
          val = this.config.value,
          paintBorders = this.config.paintBorders,
          borderType = this.config.borderType;

      var leftIcon, rightIcon, centerIcon, ixoffset, iyoffset;

      //setting gc properties are expensive, lets not do it unnecessarily

      if (val && val.constructor === Array) {
          leftIcon = val[0];
          rightIcon = val[2];
          val = val[1];
          if (typeof val === 'object') { // must be an image
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

      // draw borders
      if (paintBorders.length > 0) {
        gc.beginPath();
        if (borderType === 1)
          gc.setLineDash([4,2]); // 4px dash => 2px space
        for (var i=1; i<paintBorders.length; i++){
          gc.moveTo(x + paintBorders[i][0][0]*width, y + paintBorders[i][0][1]*height);
          gc.lineTo(x + paintBorders[i][1][0]*width, y + paintBorders[i][1][1]*height);
        }
        gc.stroke();
      }

      //draw text
      var theColor = this.config.isSelected ? this.config.fgSelColor : this.config.fgColor;
      if (gc.fillStyle !== theColor) {
          gc.fillStyle = theColor;
          gc.strokeStyle = theColor;
      }
      if (val !== null) {
          gc.fillText(val, x + halignOffset, y + valignOffset);
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

  imageCellRenderer: {
    paint: function(gc, x, y, width, height, isLink) {
      isLink = isLink || false;
      var colHEdgeOffset = this.config.properties.cellPadding,
          val = this.config.value,
          img = this.config.ASImage;

      //we must set this in order to compute the minimum width
      //for column autosizing purposes
      var textWidth = this.config.getTextWidth(gc, val);
      this.config.minWidth = textWidth + (2 * colHEdgeOffset);

      // draw image
      gc.drawImage(img, x, y);
    }
  },

  setCellRenderer() {
    let model = this._getBehavior(),
        cellProvider = model.getCellProvider(),
        self = this;
    cellProvider.getCell = function(config) {
      let renderer = cellProvider.cellCache.simpleCellRenderer,
          col = config.x + 1,
          row = config.y + 1,
          cell = Store.getCellAtLoc(col, row),
          sel = Store.getActiveSelection(),
          activeCell = Store.getActiveCell(),
          clipboard = Store.getClipboard();

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
          renderer = self.borderedCellRenderer;
          config.paintBorders = Util.getPaintedBorders(col, row, locs);
          config.bgColor = "#d3d3d3"; // light grey fill
          config.borderColor = "#000000"; // black border color
          config.borderType = 0; // default border type
        }
      }

      // clipboard highlighting
      if (clipboard && Util.isContainedInLocs(col, row, clipboard)) {
        renderer = self.copyCellRenderer;
        config.paintBorders = Util.getPaintedBorders(col, row, locs);
        config.bgColor = "#d3d3d3"; // light grey fill
        config.borderColor = "#000000"; // black border color
        config.borderType = [4,2]; // dashed border: 4px dash => 2px space
      }

      // image rendering
      if (cell.cellValue.tag === "ValueImage"){
        // console.log("setting image!");
        renderer = self.imageCellRenderer;
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
