import React from 'react';
import ActionCreator from '../actions/ASSpreadsheetActionCreators';

export default React.createClass({
  getDefaultProps() {
    return {
      behavior: 'default'
    };
  },

  getSelectionArea() {
    let hg = this.refs.hypergrid;
    let selection = hg.getSelectionModel().selections[0];
    let ul = selection.origin;
    let lr = [ul[0] + selection.width(), ul[1] + selection.height()];
    return { locs: [ul, lr], width: selection.width() + 1, height: selection.height() + 1 };
  },

  getViewingWindow() {
    let hg = this.refs.hypergrid;
    let [vs, hs] = [hg.vScrollValue, hg.hScrollValue];
    let [width, height] = [hg.getVisibleColumns(), hg.getVisibleRows()];
    return { locs: [[vs, hs], [vs + width - 1, hs + height - 1]], width: width, height: height };
  },

  //core code follows
  componentDidMount() {
    //event listeners
    let self = this;
    let hg = React.findDOMNode(this.refs.hypergrid);

    console.log(hg); //TODO: hg.addFinEventListener() not working

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
      },

      'fin-keydown': function (event) {
      },
      'fin-keyup': function (event) {
      }
    });

    for (var key in callbacks) {
      var value = callbacks[key];
      hg.addFinEventListener(key, value);
    }
  },

  render() {
    let {behavior, width, height} = this.props;

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
      <fin-hypergrid style={style} ref="hypergrid">
        {behaviorElement}
      </fin-hypergrid>
    );
  }
});
