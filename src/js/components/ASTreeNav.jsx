import React, {PropTypes} from 'react';
import {ASHorizontalDropdownButton} from './basic-controls/index.jsx';
import {FontIcon, Paper, Styles} from 'material-ui';
import Store from '../stores/ASWorkbooksStore';
import API from '../actions/ASApiActionCreators';
import _ from 'underscore';

let {Colors} = Styles;

let SheetIcon = React.createClass({
  render() {
    return (
      <FontIcon
        className="material-icons"
        style={{
          verticalAlign: 'middle',
          fontSize: '14px',
          display: 'inline-block',
          marginLeft: '5px',
          marginRight: '5px'
        }}
      >insert_drive_file</FontIcon>
    );
  }
});

let WorkbookIcon = React.createClass({
  render() {
    return (
      <FontIcon
        className="material-icons"
        style={{
          verticalAlign: 'middle',
          fontSize: '14px',
          display: 'inline-block',
          marginLeft: '5px',
          marginRight: '5px'
        }}
      >collections_bookmark</FontIcon>
    );
  }
});

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
      <Paper style={{height: '100%', paddingTop: '20px', backgroundColor: '#515151'}}>
        <ASHorizontalDropdownButton
          primary={true}
          label="New"
          height="36px"
          style={{
            display: 'block',
            margin: '0px auto 15px auto'
          }}
          buttonStyle={{
            display: 'block',
            width: '70%',
            height: '36px',
            fontSize: '12px',
            margin: '0 auto'
          }}
          onItemClick={this._onClickNew}
          menuItems={['Sheet', 'Workbook']}
        />
        {Object.keys(workbooks).map((wbid) => {
          let wb = workbooks[wbid];
          let {name, sheets, id} = wb;

          return (
            <div
              style={{
                paddingLeft: '15px',
                color: this._getTextColor(),
                verticalAlign: 'middle'
              }}
            >
              <a onClick={this._onDropdown(id)}>
                <FontIcon
                  className="material-icons"
                  style={{
                    verticalAlign: 'middle',
                    fontSize: '14px',
                    display: 'inline-block'
                  }}
                >
                  {dropdownArrowClass(id)}
                </FontIcon>
                <WorkbookIcon />
                <div style={{
                  verticalAlign: 'middle',
                  fontSize: '12px',
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
                          fontSize: '12px',
                          lineHeight: '2',
                          verticalAlign: 'middle'
                        }}
                        onClick={this._onClick(s.id)}
                      >
                        <SheetIcon />
                        <div style={{
                          verticalAlign: 'middle',
                          display: 'inline-block'
                        }}>
                          {s.name}
                        </div>
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
  },

  _onClickNew(itemTitle) {
    console.log(itemTitle);
    if (itemTitle === 'Sheet') {
      this.props.onSheetCreate();
    } else if (itemTitle === 'Workbook') {
      this.props.onWorkbookCreate();
    }
  }
});
