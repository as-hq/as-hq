import React, {PropTypes} from 'react';
import {FontIcon, Styles} from 'material-ui';
import ASDropdownButton from './ASDropdownButton.jsx';

let {Colors} = Styles;

export default React.createClass({
  contextTypes: {
    muiTheme: React.PropTypes.object
  },

  componentDidMount() {
  },

  getInitialState() {
    return {
    }
  },

  getTheme() {
    return this.context.muiTheme.component.raisedButton;
  },

  _getLabelColor() {
    return this.getTheme().textColor;
  },

  render() {
    let {width, height, iconClassName, label, menuItems} = this.props;
    return (
      <ASDropdownButton
        labelElement={
          <div
            style={{
              width: width,
              height: height
            }}
          >
            <div>
              <FontIcon
                className="material-icons"
                color={this._getLabelColor()}
                style={{
                  fontSize: height * 0.5
                }}
              >
                {iconClassName}
              </FontIcon>
            </div>
            <div>
              {label}
              <FontIcon
                className="muidocs-icon-navigation-expand-more"
                color={this._getLabelColor()}
              />
            </div>
          </div>
        }
        labelStyle={{
          padding: '0px 7px'
        }}
        width={width}
        height={height}
        menuItems={menuItems}
      />
    );
  }
});
