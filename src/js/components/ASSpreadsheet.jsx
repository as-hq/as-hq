import React from 'react';
import ActionCreator from '../actions/ASSpreadsheetActionCreators';
import Shortcuts from '../AS/Shortcuts';

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
    let lr = [ul[0] + selection.width(), ul[1] + selection.height()];
    return { topLeft: [ul.y+1, ul.x+1], width: selection.width() + 1, height: selection.height() + 1 };
  },

  makeSelection(loc) {
    // TODO
  },

  getViewingWindow() {
    let hg = this._getHypergrid();
    let [vs, hs] = [hg.vScrollValue, hg.hScrollValue];
    let [width, height] = [hg.getVisibleColumns(), hg.getVisibleRows()];
    return { locs: [[vs, hs], [vs + width - 1, hs + height - 1]], width: width, height: height };
  },

  handleKeyDown(e) {
    // console.log("keydown grid");
    // console.log(e);
    Shortcuts.killEvent(e);
    if (Shortcuts.gridShouldDeferKey(e)){ // if anything but nav keys, bubble event to parent
      this.props.onDeferredKey(e);
    } else if (!Shortcuts.tryNavShortcut(e, this._getHypergrid()))
      console.log("unhandled keydown event in grid");
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

      //event listeners
      let self = this;
      let hg = this._getHypergrid();

      let callbacks = ({
        'fin-selection-changed': function (event) {
          let { locs, width, height } = self.getSelectionArea();
          if (width === 1 && height === 1) {
            ActionCreator.selectCell(locs[0]);
          } else {
            ActionCreator.selectRange(locs);
          }
        },

        'fin-scroll-x': function (event) {
          ActionCreator.scroll(self.getViewingWindow());
        },
        'fin-scroll-y': function (event) {
          ActionCreator.scroll(self.getViewingWindow());
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

  updateCellValues(cells){
    let model = this._getModel();
    for (var key in cells){ // update the hypergrid values
      let setValue = CellConverter.cellToSetValueFormat(cells[key]);
      model.setValue(setValue[0],setValue[1], setValue[2]);
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
      <fin-hypergrid style={style} ref="hypergrid" onKeyDown={this.handleKeyDown}>
        {behaviorElement}
      </fin-hypergrid>
    );
  }
});
