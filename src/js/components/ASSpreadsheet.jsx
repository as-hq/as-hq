import React from 'react';
import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import Shortcuts from '../AS/Shortcuts';
import Converter from '../AS/Converter';
import API from '../actions/ASApiActionCreators';

export default React.createClass({

  /*************************************************************************************************************************/
  /* Default getter methods */

  _getHypergrid() {
    return React.findDOMNode(this.refs.hypergrid);
  },
  _getModel(){
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
    return { locs: [[vs, hs], [vs + cols.length - 1, hs + rows.length - 1]], width: cols.length, height: rows.length };
  },

  /*************************************************************************************************************************/
  // Display values in spreadsheet

  /* Initial a sheet with blank entries */
  initializeBlank(){
    let model = this._getModel();
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
    let model = this._getModel();
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
      Shortcuts.killEvent(e);
      this.props.onDeferredKey(e);
    } else if (Shortcuts.tryNavShortcut(e, this._getHypergrid())){ // try to handle as nav shortcut
      Shortcuts.killEvent(e); // event already handled, kill it
    } else {
      console.log("unhandled keydown event in grid");
      console.log(e);
    }
  },

  /*************************************************************************************************************************/
  // React methods

  componentDidMount() {
    document.addEventListener('polymer-ready', () => {
      this.props.onReady();
      this.initializeBlank(); 
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
          let {x, y} = self.getScroll();
          ActionCreator.scroll(x, y, self.getViewingWindow());
          },
        'fin-scroll-y': function (event) {
          let {x, y} = self.getScroll();
          ActionCreator.scroll(x, y, self.getViewingWindow());
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
  }
});
