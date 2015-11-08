import React from 'react';
import Constants from '../Constants';
import Store from '../stores/ASFindStore';
let Dropzone = require('react-dropzone');
import {FlatButton,Styles} from 'material-ui';
let request = require('superagent');


export default React.createClass({

  propTypes: {
  	style: React.PropTypes.object.isRequired
  },

  _onDrop(files){
    console.log('Received files: ', files);
    let req = request
      .post(Constants.HOST_STATIC_URL_FOR_FILES);
    files.forEach((file) => {
      req.attach(file.name, file);
    });
    req.end();
  },
 
  render(){
    let buttonStyle = {
      position:'absolute',
      width:this.props.style.width,
      height:this.props.style.height
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
