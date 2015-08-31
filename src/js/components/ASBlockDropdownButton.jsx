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
                className={iconClassName}
                color={Colors.grey50}
                style={{
                  fontSize: height * 0.5
                }}
              />
            </div>
            <div>
              {label}
              <FontIcon
                className="muidocs-icon-navigation-expand-more"
                color={Colors.grey50}
              />
            </div>
          </div>
        }
        width={width}
        height={height}
        menuItems={menuItems}
      />
    );
  }
});
