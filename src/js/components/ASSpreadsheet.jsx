import React from 'react';
import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import Shortcuts from '../AS/Shortcuts';
import CellConverter from '../AS/CellConverter';
import API from '../actions/ASApiActionCreators';

export default React.createClass({

  /*************************************************************************************************************************/
  // Default getter methods

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
    let range = {row: ul.y+1,
              col: ul.x+1,
              row2: ul.y + selection.height()+1,
              col2: ul.x + selection.width()+1};
    if (range.row === range.row2 && range.col === range.col2)
      return {
        width: selection.width() + 1,
        height: selection.height() + 1,
        range: {row: range.row, col: range.col}
      };
    else return {
        width: selection.width() + 1,
        height: selection.height() + 1,
        range:range
    };
  },

  // Returns the position of scroll
  getScroll() {
    let hg = this._getHypergrid();
    return {x: hg.hScrollValue, y: hg.vScrollValue};
  },

  makeSelection(loc) {
    // TODO
  },

  getViewingWindow() {
    let hg = this._getHypergrid();
    let [vs, hs] = [hg.vScrollValue, hg.hScrollValue];
    let [cols, rows] = [hg.getVisibleColumns(), hg.getVisibleRows()];
    return { locs: [[vs, hs], [vs + cols.length - 1, hs + rows.length - 1]], width: cols.length, height: rows.length };
  },

  /*************************************************************************************************************************/
  // Display values in spreadsheet

  setDisplayValue(val) {
    let model = this._getModel();
    model.setValue(val.index.col - 1, val.index.row - 1, val.display);
  },

  initializeBlank(){
    let model = this._getModel();
    model.getValue = function(x, y) {
      return '';
    };
  },

  updateCellValues(cells){
    let model = this._getModel();
    for (var key in cells){ // update the hypergrid values
      let val = CellConverter.cellToGridValue(cells[key]);
      console.log("Updating display value: " + JSON.stringify(val));
      this.setDisplayValue(val);
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
