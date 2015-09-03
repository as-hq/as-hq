import React, {PropTypes} from 'react';
import {Paper, Styles} from 'material-ui';
import Store from '../stores/ASWorkbooksStore';
import API from '../actions/ASApiActionCreators';
import _ from 'underscore';

let {Colors} = Styles;

export default React.createClass({
  contextTypes: {
    muiTheme: PropTypes.object
  },

  getInitialState() {
    return { workbooks: [], open: {} };
  },

  componentDidMount() {
    Store.addChangeListener(this._onChange);
    API.sendGetWorkbooks();
  },

  componentWillUnmount() {
    Store.removeChangeListener(this._onChange);
  },

  render() {
    let {workbooks, open} = this.state;
    let dropdownArrowClass = (id) => open[id] ? "side_dropdown_arrow" : "dropdown_arrow";

    return (
      <Paper>
        {workbooks.map((wb) =>
          <div>
            <span className={dropdownArrowClass(wb.id)} onClick={this._onDropdown(wb.id)}></span>
            {wb.name}
            {
              open[wb.id] ? (
                <div style={{marginLeft: '15px'}}>
                  {wb.sheets.map((s) => //TODO make sure it's a list
                    <div>
                      {s.name}
                    </div>
                  )}
                </div>
              ) : null
            }
          </div>
        )}
      </Paper>
    );
  },

  _onChange() {
    //TODO merge instead of set
    this.setState({ workbooks: Store.getWorkbooks() });
  },

  _onDropdown(id) {
    return (() => {
      let {open} = _.extend({}, this.state);
      open[id] = !open[id];
      this.setState({ open: open });
    });
  }
});
