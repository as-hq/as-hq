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

let HoverIndicator = React.createClass({
  render() {
    let {color} = this.props;

    return (
      <div
        style={{
          display: 'block',
          height: '100%',
          width: '4px',
          float: 'left',
          backgroundColor: color
        }}
      />
    );
  }
});

export default React.createClass({
  contextTypes: {
    muiTheme: PropTypes.object
  },

  getInitialState() {
    return { workbooks: [], open: {}, hoverItem: null, hovering: false };
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
                color: this._getTextColor(),
                verticalAlign: 'middle'
              }}
            >
              <div
                style={{ height: '24px', cursor: 'pointer' }}
                onMouseOver={this._onMouseOverNavItem(id)}
                onMouseOut={this._onMouseOutNavItem(id)}
              >
                <HoverIndicator color={this._isHovered(id) ? Colors.pink700 : null} />
                <div
                  onClick={this._onDropdown(id)}
                  style={{
                    display: 'inline-block',
                    paddingLeft: '10px',
                    width: 'calc(100% - 4px)',
                    backgroundColor: this._isHovered(id) ? Colors.grey700 : null
                  }}>
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
                </div>
              </div>
              {
                open[id] ? (
                  <div>
                    {sheets.map((s) => //TODO make sure it's a list
                      <div
                        onMouseOver={this._onMouseOverNavItem(s.id)}
                        onMouseOut={this._onMouseOutNavItem(s.id)}
                        style={{
                          height: '24px',
                          cursor: 'pointer'
                        }}>
                        <HoverIndicator color={this._isHovered(s.id) ? Colors.pink700 : null} />
                        <div
                          style={{
                            display: 'inline-block',
                            fontSize: '12px',
                            lineHeight: '2',
                            verticalAlign: 'middle',
                            paddingLeft: '41px',
                            width: 'calc(100% - 4px)',
                            backgroundColor: this._isHovered(s.id) ? Colors.grey700 : null
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
  },

  _onMouseOverNavItem(id) {
    return () => {
      this.setState({ hoverItem: id, hovering: true });
    };
  },

  _onMouseOutNavItem(id) {
    return () => {
      this.setState({ hovering: false });
    };
  },

  _isHovered(id) {
    return this.state.hovering && this.state.hoverItem == id;
  }
});
