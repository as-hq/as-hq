import React from 'react';
import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import Shortcuts from '../AS/Shortcuts';
import CellConverter from '../AS/CellConverter';
import API from '../actions/ASApiActionCreators';

export default React.createClass({
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
    let lr = {row: ul.y + selection.height()+1, col: ul.x + selection.width()+1};
    return { locs: [{row: ul.y+1, col: ul.x+1}, lr], width: selection.width() + 1, height: selection.height() + 1 };
  },

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

  //core code follows
  componentDidMount() {
    document.addEventListener('polymer-ready', () => {
      this.props.onReady();

      // initialize sheet to be blank
      let model = this._getModel();
      model.getValue = function(x, y) {
          return '';
      };

      // init viewing window values
      // let vwindow = this.getViewingWindow();
      // console.log("initializing sheet with range: " + JSON.stringify(vwindow));
      // API.getCells(vwindow.locs);

      //event listeners
      let self = this;
      let hg = this._getHypergrid();

      let callbacks = ({
        'fin-selection-changed': function (event) {
          let { locs, width, height } = self.getSelectionArea();
          if (width === 1 && height === 1)
            self.props.onSelectionChange(locs);
        },

        'fin-scroll-x': function (event) {
          let {x, y} = self.getScroll();
          ActionCreator.scroll(x, y, self.getViewingWindow());
        },
        'fin-scroll-y': function (event) {
          let {x, y} = self.getScroll();
          ActionCreator.scroll(x, y, self.getViewingWindow());
        }

        // 'fin-keydown': self.handleKeyDown,

        // 'fin-keyup':
      });

      for (var key in callbacks) {
        var value = callbacks[key];
        hg.addFinEventListener(key, value);
      }
    });
  },

  setDisplayValue(val) {
    let model = this._getModel();
    model.setValue(val.index.col - 1, val.index.row - 1, val.display);
  },

  updateCellValues(cells){
    let model = this._getModel();
    for (var key in cells){ // update the hypergrid values
      let val = CellConverter.cellToGridValue(cells[key]);
      // console.log("display value: " + JSON.stringify(val));
      this.setDisplayValue(val);
    }
    model.changed(); // causes hypergrid to show updated values
  },

  render() {
    let {behavior, width, height} = this.props; //should also have onReady

    let style = {
      width: width, height: height
    };

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
