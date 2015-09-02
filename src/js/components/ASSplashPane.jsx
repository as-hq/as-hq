import React from 'react';
import Store from '../stores/ASEvaluationStore';
import API from '../actions/ASApiActionCreators';
import Constants from '../Constants';
import Util from '../AS/Util';

export default React.createClass({

  componentDidMount() {
    // TODO
  },

  handleClick() {
    console.log("splash clicked");
    this.props.onProceed('eval', {}); // TODO send initial info object with the opened sheet
  },

  render() {
    let style = {
      width: '100%',
      height: this.props.height + "px",
      backgroundColor: Util.colorToHtml('darkgray')
    };
    return (
      <div onClick={this.handleClick} style={style}>
      No files open. [Debug: click to continue to grid.]
      </div>
    );
  }

});
