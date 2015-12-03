import {logDebug} from '../AS/Logger';

import React from 'react';
import Constants from '../Constants';
import {HOST_IP, HOST_BASE_URL} from '../Constants';

import Store from '../stores/ASFindStore';
import Dropzone from 'react-dropzone';
import {FlatButton,Styles} from 'material-ui';
let request = require('superagent');


export default React.createClass({

  propTypes: {
  	style: React.PropTypes.object.isRequired
  },

  getStaticUrl() {
    if (Constants.isRemote) {
      return 'http://' + HOST_IP + ':9000';
    } else {
      return 'http://' + HOST_BASE_URL + ':9000';
    }
  },

  _onDrop(files) {
    logDebug('Received files: ', files);
    let req = request
      .post(this.getStaticUrl());
    files.forEach((file) => {
      req.attach(file.name, file);
    });
    req.end();
  },

  render() {
    let buttonStyle = {
      position:'absolute',
      width: this.props.style.width,
      height: this.props.style.height
    };
    return (
    	<Dropzone onDrop={this._onDrop} style={this.props.style} >
        <FlatButton
              label="IMPORT"
              hoverColor={Styles.Colors.pink700}
              onClick={null}
              style={buttonStyle} />
      </Dropzone>
    );
  }

});
