import React from 'react';
import Constants from '../Constants';
import ASCheckedButton from './basic-controls/ASCheckedButton.jsx';
import Store from '../stores/ASFindStore';

import {TextField, FlatButton, FontIcon, Styles} from 'material-ui';

/*
The point of this find bar (see gsheets) is to provide a fast default find method (only on values, current sheet, no case)
*/

export default React.createClass({

  propTypes: {
  	onEnter: React.PropTypes.func.isRequired,
  	onNext: React.PropTypes.func.isRequired,
  	onPrev: React.PropTypes.func.isRequired,
  	onClose: React.PropTypes.func.isRequired,
  	onModal: React.PropTypes.func.isRequired
  },

  getInitialState() {
  	return {
  		// Don't set to 0; otherwise you lose position state upon reloading find bar
  		// This is for the "1 of 5" stuff
  		pos:CellStore.getFindPos(),
  		total:CellStore.getFindTotal()
  	};
  },

  componentDidMount() {
  	// Place focus in the find textarea upon mounting
  	this.refs.findText.focus();
  	CellStore.addChangeListener(this.handleChange);
  },

  componentWillUnmount() {
  	CellStore.removeChangeListener(this.handleChange);
  },

  // Respond to a find response from backend
  handleChange() {
  	this.setState({pos:CellStore.getFindPos(),total:CellStore.getFindTotal()});
  },

  _onClose() {
  	CellStore.setFindText(this.refs.findText.getValue());
  	this.props.onClose();
  },

  _onEnter() {
  	let curText = this.refs.findText.getValue();
  	if (CellStore.getFindText() === curText) { // increment 
  		this.props.onNext();
  	} else {
  		CellStore.setFindText(curText);
  		this.props.onEnter();
  	}
  },

  _onModal() {
  	CellStore.setFindText(this.refs.findText.getValue());
  	this.props.onModal();
  },

  render() {
  	let buttonStyle = {
  		left:"5px",
  		display:"inline-block",
  		verticalAlign:"middle",
 	    width:"10%",
 	    minWidth:"10%",
 	    height:"100%",
 	    backgroundColor:Styles.Colors.grey900,
  	};
  	let textStyle = {
  		position:'relative',
  		display:"inline-block",
  		verticalAlign:"middle",
	    width:"40%",
	    height:"100%",
	    fontFamily:  'Roboto, sans-serif' //'"Lucida Console", Monaco, monospace'
  	};
  	let matchStyle = {
  		left:"5px",
  		position:'relative',
  		display:"inline-block",
  		verticalAlign:"middle",
	    width:"20%",
	    height:"100%",
	    fontFamily: 'Roboto, sans-serif'
  	};
  	let iconStyle = {
  		verticalAlign:"middle"
  	};

  	let position = (this.state.total === 0) ? "0 of 0" : ((this.state.pos+1) + " of " + this.state.total);
  	return (
	  	<div style={{
          position:'absolute',
          width:'30%',
          height:40,
          top:'0%',
          left:'70%',
          zIndex:10,
          backgroundColor:Styles.Colors.grey900
          }} >
	  		<TextField ref="findText" style={textStyle} hintText="Find Text" defaultValue={CellStore.getFindText()}
	  				   underlineStyle={{borderColor: Styles.Colors.amber900}}
					   onEnterKeyDown={this._onEnter} />
	  		<TextField style={matchStyle} disabled={true} hintText={position}
					   onEnterKeyDown={this.props.onEnterKeyDown} />
	  		<FlatButton style={buttonStyle} onClick={this.props.onNext}>
				<FontIcon style={iconStyle} className="material-icons" color={Styles.Colors.grey50}>
					{"keyboard_arrow_down"}
				</FontIcon>
        	</FlatButton>
        	<FlatButton style={buttonStyle} onClick={this.props.onPrev} >
				<FontIcon style={iconStyle} className="material-icons" color={Styles.Colors.grey50}>
					{"keyboard_arrow_up"}
				</FontIcon>
        	</FlatButton>
        	<FlatButton style={buttonStyle} onClick={this._onModal} >
				<FontIcon style={iconStyle} className="material-icons" color={Styles.Colors.grey50}>
					{"more_horiz"}
				</FontIcon>
        	</FlatButton>
        	<FlatButton style={buttonStyle} onClick={this._onClose} >
				<FontIcon style={iconStyle} className="material-icons" color={Styles.Colors.grey50}>
					{"close"}
				</FontIcon>
        	</FlatButton>
	  	</div>
	);
  }

});
