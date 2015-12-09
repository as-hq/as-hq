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
    let {style} = this.props;
    let buttonStyle = {
      width: this.props.style.width,
      height: this.props.style.height,
      display: 'inline-block',
      ...style
    };

    return (
      // 12-09-2015 Michael: Ritesh says that the onDrop thing actually
      // gets triggered by opening a file in the file browser that pops up.
      // That's why onClick is null for the Flatbutton - because clicking
      // on the Dropzone not the button triggers it.
    	<Dropzone onDrop={this._onDrop} style={{
        transform: 'translateY(-50%)',
        ...buttonStyle
      }} >
        <FlatButton
              label="IMPORT"
              hoverColor={Styles.Colors.pink700}
              onClick={null}
              style={buttonStyle} />
      </Dropzone>
    );
  }

});
