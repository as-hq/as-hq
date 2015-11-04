import {logDebug} from '../AS/Logger';

import React from 'react';
import Constants from '../Constants';
let Draggable = require('react-draggable');
import Store from '../stores/ASFindStore';
import {TextField, AppBar, DropDownMenu, FlatButton, IconButton, FontIcon, Styles, Paper, Tab, Tabs, Menu, IconMenu} from 'material-ui';
let MenuItem = require('material-ui/lib/menus/menu-item');
let MenuDivider = require('material-ui/lib/menus/menu-divider');

import ASHorizontalDropdownButton from './basic-controls/ASHorizontalDropdownButton.jsx';

/* NOTES:
using react-draggable around this is buggy; it jumps around while dragging and text selection drags as well
The point of this modal is to provide more options for find and replace (not yet implemented)
  case
  sheet
  full contents (the PayloadFind on backend covers these cases, and so does Excel)
replacing will be done on frontend and and eval request sent to backend after sending a find to backend
*/

export default React.createClass({

  propTypes: {
  	initialSelection:React.PropTypes.number.isRequired,
  	onClose: React.PropTypes.func.isRequired
  },
  click(a){
  	logDebug("CLICK " + a );
  },
  _onClose(){
  	this.props.onClose();
  },
  render(){
  	let button = <IconButton >
  			<FontIcon className="muidocs-icon-custom-sort"/>
		</IconButton>;
  	let textStyle = {
	    width:'60%',
	    paddingLeft:'20%',
	    fontFamily:  'Roboto, sans-serif'
  	};
  	let buttonStyle = {
	    width:'80%',
	    height:'40px',
	    paddingLeft:'20%',
	    paddingTop:'70px'
  	};
  	let iconStyle = {
  		verticalAlign:"middle"
  	};

  	let findButtons = <div style={{
        	          textAlign:'center',
        	          width:'100%',
        	          height:40
        	          }} >
        		  		<FlatButton style={{width:"20%",height:"100%",display:'inline-block'}} onClick={this.props.onNext}>
        					<FontIcon style={iconStyle} className="material-icons" color={Styles.Colors.blue50}>
        						{"keyboard_arrow_down"}
        					</FontIcon>
        	        	</FlatButton>
        	        	<FlatButton style={{width:"20%",height:"100%",display:'inline-block'}} onClick={this.props.onPrev} >
        					<FontIcon style={iconStyle} className="material-icons" color={Styles.Colors.blue50}>
        						{"keyboard_arrow_up"}
        					</FontIcon>
        	        	</FlatButton>

        		  	</div>;

    return (

        <div style={{position:'absolute',zIndex:10,left:600,top:200}} >

            <Paper zDepth={5} style={{width:500,height:500,borderStyle:'solid 2px'}}>
            	<AppBar style={{width:"100%",height:"10px",backgroundColor:Styles.Colors.amber900}}
            	  title="Find and Replace (TODO)"
            	  showMenuIconButton={false}
            	  iconElementRight={<FlatButton onClick={this._onClose} >
            		<FontIcon style={iconStyle} className="material-icons" color={Styles.Colors.grey900}>
            			{"close"}
            		</FontIcon>
                  </FlatButton>} />
              <Tabs initialSelectedIndex={this.props.initialSelection}
              style={{backgroundColor:Styles.Colors.grey900, width:"100%",height:"100%"}}>
                <Tab label="Find" >
                	<TextField style={textStyle}
                			   defaultValue=""
                			   floatingLabelText="Find Text"
                			   underlineStyle={{borderColor: Styles.Colors.amber900}}/>
                	{findButtons}
                </Tab>
                <Tab label="Replace" >
                	<TextField style={textStyle}
                			   defaultValue=""
                			   floatingLabelText="Find Text"
                			   underlineStyle={{borderColor: Styles.Colors.amber900}}/>

                	<TextField style={textStyle}
                			   defaultValue=""
                			   floatingLabelText="Replace Text"
                			   underlineStyle={{borderColor: Styles.Colors.amber900}}/>
                </Tab>
              </Tabs>
            </Paper>

       	</div>
    );
  }


});

/*
<TextField style={textStyle}
              				   defaultValue=""
              				   floatingLabelText="Find Text"
              				   underlineStyle={{borderColor: Styles.Colors.amber900}}/>

	              	<DropDownMenu
	              	  menuItems={[{payload:0,text:"Search in values"},
	              	  	{payload:1,text:"Search in formulas"},
	              	  	{payload:2,text:"Search in both"}]}
	              	  underlineStyle={{ display: 'none' }}
	              	  style={buttonStyle}/>


	              	<div style={textStyle}>
	              		<input style={{width:"15px",height:"15px"}} type="checkbox" />
	              		 <TextField style={textStyle}
           				   defaultValue="Match with case"
           				   disabled={true}
           				   underlineDisabledStyle={{borderColor: Styles.Colors.amber900, borderStyle:'solid 1px'}}/>
	              	</div>
	              	*/
