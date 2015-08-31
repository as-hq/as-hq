import React, {PropTypes} from 'react';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import ASButton from './ASButton.jsx';
import ASCheckedButton from './ASCheckedButton.jsx';
import ASDropdown from './ASDropdown.jsx';
import ASBlockDropdownButton from './ASBlockDropdownButton.jsx';
import ASHorizontalDropdownButton from './ASHorizontalDropdownButton.jsx';
import {AppCanvas, Paper, Styles} from 'material-ui';

const ThemeManager = new Styles.ThemeManager();

export default React.createClass({
  componentWillMount() {
    ThemeManager.setTheme(ThemeManager.types.DARK);
  },

  propTypes: {
  },

  getDefaultProps() {
    return {
      tasks: []
    }
  },

  childContextTypes: {
    muiTheme: React.PropTypes.object
  },

  getChildContext() {
    return {
      muiTheme: ThemeManager.getCurrentTheme()
    };
  },

  render() {
    return (
      <div className="full">
        <Paper>
          <ASHorizontalDropdownButton
            iconClassName="muidocs-icon-action-home"
            label="Home"
            menuItems={[ 'item1', 'item2' ]}
          />
          <ASBlockDropdownButton
            width={60}
            height={88}
            iconClassName="muidocs-icon-action-home"
            label="Home"
            menuItems={[ 'item1', 'item2' ]}
          />
          <ASDropdown
            menuItems={[
              { payload: '1', text: 'test1' },
              { payload: '2', text: 'test2' }
            ]}
          />
          <ASCheckedButton
            iconClassName="muidocs-icon-action-home"
            onCheckChange={this._onCheckChange}
          />
        </Paper>
      </div>
    );
  },

  _onCheckChange(e) {
    console.log('test', e);
  }
});
