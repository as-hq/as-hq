import React from 'react';
import ActionCreator from '../actions/SpreadsheetActionCreators';

export default React.createClass({
  getSelectionArea() {
    let hg = this.refs.hypergrid;
    let selection = hg.getSelectionModel().selections[0];
    let ul = selection.origin;
    let lr = [ul[0] + selection.width(), ul[1] + selection.height()];
    return [ul, lr];
  },

  //core code follows
  componentDidMount() {
    //event listeners
    let hg = this.refs.hypergrid;
    let callbacks = ({
      'fin-selection-changed': function (event) {
      },
      'fin-scroll-x': function (event) {
      },
      'fin-scroll-y': function (event) {
      },
      'fin-click': function (event) {
      },
      'fin-double-click': function (event) {
      },
      'fin-keydown': function (event) {
      },
      'fin-keyup': function (event) {
      }
    });

    callbacks.forEach((value, key) => hg.addFinEventListener(key, value));
  },

  render() {
    let {behavior} = this.props;
    let behaviorElement;
    switch (behavior) {
      case 'json':
        behaviorElement = <fin-hypergrid-behavior-json />;
        break;
      case 'default':
        behaviorElement = <fin-hypergrid-behavior-default />
    }

    return (
      <fin-hypergrid ref="hypergrid">
        {behaviorElement}
      </fin-hypergrid>
    );
  }
});
