/* @flow */

import React from 'react';
import Util from '../../AS/Util';

import type {
  StoreLink
} from '../../types/React';

import Store from '../../stores/ShortcutHelperStore';
import ActionCreator from '../../actions/ShortcutHelperActionCreators';

// $FlowFixMe
import {Paper, AppBar} from 'material-ui';
// $FlowFixMe
import Colors from 'material-ui/lib/styles/colors';
// $FlowFixMe
import Divider from 'material-ui/lib/divider';

// $FlowFixMe
import Table from 'material-ui/lib/table/table';
// $FlowFixMe
import TableHeaderColumn from 'material-ui/lib/table/table-header-column';
// $FlowFixMe
import TableRow from 'material-ui/lib/table/table-row';
// $FlowFixMe
import TableHeader from 'material-ui/lib/table/table-header';
// $FlowFixMe
import TableRowColumn from 'material-ui/lib/table/table-row-column';
// $FlowFixMe
import TableBody from 'material-ui/lib/table/table-body';
// $FlowFixMe
import IconButton from 'material-ui/lib/icon-button';
// $FlowFixMe
import NavigationClose from 'material-ui/lib/svg-icons/navigation/close';
// $FlowFixMe
import FlatButton from 'material-ui/lib/flat-button';
// $FlowFixMe
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe
import LightTheme from 'material-ui/lib/styles/raw-themes/light-raw-theme';

type Shortcut = {
	name: string;
	keyCode: string;
};

type ShortcutGroup = {
	name: string;
	group: Array<Shortcut>;
};

const styles: any = {
  root: {
    position: 'absolute',
    width: 450, 
    bottom: 28,
    right: 0,
    top: 152,
    zIndex: 10000,
    overflow: 'auto' // scroll bar
  }, 
  appBar: {
    position: 'absolute',
    backgroundColor: Colors.grey900,
    width: 450, 
    top: 88,
    right: 0,
    zIndex: 10000
  }, 
  header: {
    color: Colors.cyan500,
    textAlign: 'center',
    fontSize: '18px'
  },
  left: {
    backgroundColor: Colors.grey400, 
    color: 'black', 
    textAlign: 'center',
    fontSize: '17px'
  },
  right: {
    backgroundColor: Colors.grey100, 
    color: 'black', 
    textAlign: 'center',
    fontSize: '17px'
  }
};

const shortcutGroups: Array<ShortcutGroup> =
[
  {
    name: 'Common', 
    group: [
      {name: 'Copy', keyCode: 'Ctrl+C'},
      {name: 'Copy code above', keyCode: 'Ctrl+Shift+\\'},
      {name: 'Copy value above', keyCode: 'Ctrl+\\'},
      {name: 'Paste', keyCode: 'Ctrl+V'},
      {name: 'Find', keyCode: 'Ctrl+F'},
      {name: 'Escape', keyCode: 'Esc'},
      {name: 'Delete', keyCode: 'Backspace, Delete'},
      {name: 'Undo', keyCode: 'Ctrl+Z'},
      {name: 'Redo', keyCode: 'Ctrl+Shift+Z'},
      {name: 'Repeat last action', keyCode: 'Ctrl+Y'},
      {name: 'Save', keyCode: 'Ctrl+S'}
    ]
  }, 
  {
    name: 'Navigation', 
    group: [
      {name: 'Move to data boundary', keyCode: 'Ctrl+Up/Down/Left/Right'},
      {name: 'Move window up', keyCode: 'PageUp'},
      {name: 'Move window down', keyCode: 'PageDown'},
    ]
  }, 
  {
    name: 'Selection', 
    group: [
      {name: 'Select data boundary', keyCode: 'Ctrl+Shift+Up/Down/Left/Right'},
      {name: 'Select all', keyCode: 'Ctrl+A'},
      {name: 'Select row', keyCode: 'Shift+Space'},
      {name: 'Select column', keyCode: 'Ctrl+Space'}
    ]
  }, 
  {
    name: 'Evaluation', 
    group: [
      {name: 'Eval, move down', keyCode: 'Enter'},
      {name: 'Eval, move right', keyCode: 'Tab'},
      {name: 'Eval, move up', keyCode: 'Shift+Enter'},
      {name: 'Eval, move left', keyCode: 'Shift+Tab'},
      {name: 'Eval (editor)', keyCode: 'Ctrl+Enter'},
      {name: 'Eval array formula', keyCode: 'Ctrl+Shift+Enter'},
      {name: 'Eval vertical range', keyCode: 'Ctrl+D'},
      {name: 'Eval horizontal range', keyCode: 'Ctrl+R'},
      {name: 'Set language Excel', keyCode: 'Ctrl+1'},
      {name: 'Set language Python', keyCode: 'Ctrl+2'},
      {name: 'Set language R', keyCode: 'Ctrl+3'},
      {name: 'Set language SQL', keyCode: 'Ctrl+4'}
    ]
  }, 
  {
    name: 'Formatting', 
    group: [
      {name: 'Bold', keyCode: 'Ctrl+B'},
      {name: 'Italic', keyCode: 'Ctrl+I'},
      {name: 'Format value', keyCode: 'Ctrl+Shift+2/3/4/5/6'},
      {name: 'Open cond formatting', keyCode: 'Ctrl+Shift+F'}
    ]
  }, 
  {
    name: 'Miscellaneous', 
    group: [
      {name: 'Make chart', keyCode: 'F11'},
      {name: 'Toggle header file', keyCode: 'Alt+H'},
      {name: 'Toggle shortcut helper', keyCode: 'Ctrl+Q'},
      {name: 'Toggle focus', keyCode: 'F2'},
      {name: 'Toggle rel/abs references', keyCode: 'F4'}
    ]
  }
];

export default class ShortcutHelper extends React.Component<{}, {}, {}> {

  constructor(props: {}) {
    super(props);
  }

  $storeLinks: Array<StoreLink>;

  componentDidMount() {
    Util.React.addStoreLinks(this, [{ store: Store }]);
  }

  componentWillUnmount() {
    Util.React.removeStoreLinks(this);
  }

  _onRequestClose() {
    ActionCreator.closeShortcutHelper();
  }

  getChildContext(): any {
    return {
      muiTheme: ThemeManager.getMuiTheme(LightTheme)
    };
  }

  renderTable(shortcutGroup: ShortcutGroup): React.Element {
  	const {name, group} = shortcutGroup;
  	return (
  		<Table>
  		   <TableHeader adjustForCheckbox={false} displaySelectAll={false} >
	       	  <TableRow>
	       	    <TableHeaderColumn style={styles.header}> 
	       	    	{name}
	       	    </TableHeaderColumn>
	       	  </TableRow>
	       	</TableHeader>
  		   <TableBody displayRowCheckbox={false} >
  		   	{
  		   		group.map((shortcut) => 
  		   			<TableRow>
  		   			  <TableRowColumn style={styles.left}> 
                  {shortcut.name} 
                </TableRowColumn>
  		   			  <TableRowColumn style={styles.right}> 
                  {shortcut.keyCode} 
                </TableRowColumn>
  		   			</TableRow>
  		   		)
  		   	}
  		   </TableBody>
  		 </Table>
  	);
  }

  render(): React.Element {
    const display = Store.isShortcutHelperOpen() ? 'block' : 'none';
    return (
    	<div style={{display}}>
	  		<AppBar
	  			style={styles.appBar}
			    title={<span>Keyboard Shortcuts</span>}
			    showMenuIconButton={false}
			    iconElementRight={
			    	<IconButton onClick={(e) => this._onRequestClose()}>
			    		<NavigationClose />
			    	</IconButton>
			    }/>
	    	<Paper
	    		style={styles.root}>
	    		{shortcutGroups.map((shortcutGroup) => this.renderTable(shortcutGroup))}
	    	</Paper>
	    </div>
    );
  }

}

ShortcutHelper.childContextTypes = {
  muiTheme: React.PropTypes.object
};