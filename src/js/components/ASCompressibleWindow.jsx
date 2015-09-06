import React from 'react'
import {Button} from 'react-bootstrap'
import SlidableRightSidebar from './repl/SlidableRightSidebar.jsx'
import Window from './repl/Window.jsx'
import AceEditor from './AceEditor.jsx'
import {Tab,Tabs} from 'react-bootstrap'
import {AppBar,FlatButton,Toolbar,DropDownMenu,Styles} from 'material-ui'
let NavigationClose = require('material-ui/lib/svg-icons/navigation/close');
import Constants from '../Constants';

import  ASEvaluationPane from './ASEvaluationPane.jsx'


export default React.createClass({
  getInitialState(){
    return {
      open: false,
      val:""
    };
  },
  _onClick(){
    console.log("clicked comp button");
    this.setState({open: !this.state.open});
   
  },

  render(){

    var leftComp = <ASEvaluationPane behavior="default" ref="evalPane"/> 
    ;

    let languages = [];
    for (var key in Constants.Languages) {
      languages.push({
        payload: Constants.Languages[key],
        text: Constants.Languages[key].Display
      });
    }

    var sidebarContent = 
      <div style={{width:"100%",height:"100%",backgroundColor:'red',marginLeft:"6px"}} >
        <AppBar title="REPL" style={{backgroundColor:Styles.Colors.grey900}} showMenuIconButton={false} iconElementRight={<FlatButton onClick={this._onClick}><NavigationClose /></FlatButton>} />
        <Toolbar
          style={{backgroundColor: Styles.Colors.grey700}}
          showMenuIconButton={false} >
          <DropDownMenu
            menuItems={languages}
            onChange={function(){}}
            underlineStyle={{ display: 'none' }} />
        </Toolbar>
        <AceEditor ref="editor" 
          language="excel"
          theme="monokai"
          height="100%" name="excel_repl" value={this.state.val}>
        </AceEditor>
      </div>;


      return (
          <div style={{width:"100%",height:"100%"}}>
            <Button onClick={this._onClick}> click </Button>
            <SlidableRightSidebar leftComp={leftComp} sidebar={sidebarContent} docked={this.state.open}>
            </SlidableRightSidebar>
          </div>
      );
    }

});

