import React, {PropTypes} from 'react';
import {FontIcon, Paper, Styles} from 'material-ui';
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

  _getTheme() {
    return this.context.muiTheme.component.raisedButton;
  },

  _getTextColor() {
    return this._getTheme().textColor;
  },

  render() {
    let {workbooks, open} = this.state;
    let dropdownArrowClass = (id) => open[id] ? "keyboard_arrow_down" : 'keyboard_arrow_right';

    return (
      <Paper style={{height: '100%', paddingTop: '20px'}}>
        {Object.keys(workbooks).map((wbid) => {
          let wb = workbooks[wbid];
          let {name, sheets, id} = wb;

          return (
            <div
              style={{
                marginLeft: '15px',
                color: this._getTextColor(),
                verticalAlign: 'middle'
              }}
            >
              <a onClick={this._onDropdown(id)}>
                <FontIcon
                  className="material-icons"
                  style={{
                    verticalAlign: 'middle',
                    fontSize: '18px',
                    display: 'inline-block'
                  }}
                >
                  {dropdownArrowClass(id)}
                </FontIcon>
                <div style={{
                  verticalAlign: 'middle',
                  fontSize: '14px',
                  lineHeight: '2',
                  display: 'inline-block'
                }}>
                  {name}
                </div>
              </a>
              {
                open[id] ? (
                  <div style={{marginLeft: '30px'}}>
                    {sheets.map((s) => //TODO make sure it's a list
                      <div
                        style={{
                          fontSize: '14px',
                          lineHeight: '2'
                        }}
                        onClick={this._onClick(s.id)}
                      >
                        {s.name}
                      </div>
                    )}
                  </div>
                ) : null
              }
            </div>
          );
        })}
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
  },

  _onClick(id) {
    return (() => {
      this.props.onDocumentOpen(id);
    });
  }
});
