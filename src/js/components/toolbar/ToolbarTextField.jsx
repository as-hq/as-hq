import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarButton from './ToolbarButton.jsx';
import DropDownArrow from 'material-ui/lib/svg-icons/navigation/arrow-drop-down';

let Tooltip = require("react-tooltip");


export default React.createClass({

  /*************************************************************************************************************************/
  // Prop and state methods

  getInitialState() {
    return {
      cursor: 'auto' // cursor icon over text field
    }
  },

  /* We need the following props
    1) displayValue to be shown in the text field
    2) onClick callback
    3,4,5) size params
    6) tooltip to display as hint
    7) should we show tooltip
    8) arrowSize: size of the arrow icon (square)
  */
  propTypes: {
    displayValue: React.PropTypes.string.isRequired,
    onClick: React.PropTypes.func,
    width: React.PropTypes.number,
    height: React.PropTypes.number,
    spacing: React.PropTypes.number,
    tooltip: React.PropTypes.string,
    showTooltip: React.PropTypes.bool,
    arrowSize: React.PropTypes.number
  },

  getDefaultProps() {
    return {
      width: 100,
      height: 36,
      spacing: 0, // most of the time there's a separator right before this element, so no spacing required
      onClick: (active) => {},
      showTooltip: true,
      arrowSize: 15
    };
  },

  /*************************************************************************************************************************/
  // Responding to events

  _onMouseOver(e) {
    this.setState({cursor: 'pointer'});
  },

  /*************************************************************************************************************************/
  // Styles and rendering

  getStyles() {
    return {
      outer: {
        display: 'inline-block', // should stack horizontally with other toolbar components
        position: 'relative', 
        top: '50%', 
        transform: 'translateY(-50%)', 
        width: this.props.width, 
        height: this.props.height,
        cursor: this.state.cursor,
        marginLeft: this.props.spacing, // spacing 
      },
      arrow: { // arrow should be towards the right
        position: 'absolute',
        fill: Styles.Colors.grey50,
        width: this.props.arrowSize,
        right: '10%',
        top: '25%'
      },
      label: { // the label should be vertically centered, decent font size, a bit to the left
        position: 'absolute',
        color: Styles.Colors.grey50,
        top: '50%',
        transform: 'translateY(-50%)',
        fontSize: '16px',
        left: '10%',
      },
      underline: { // underline should be horizontally centered, a bit above the bottom
        position: 'absolute',
        borderTop: `solid 1px ${Styles.Colors.grey500}`,
        bottom: '20%',
        width: '80%',
        left: '10%'
      },
    };
  },

  // Return a component with underline, arrow, and text, and a tooltip if applicable
  // There's some redundancy that can probably be eliminated with React helpers
  render() {
    let styles = this.getStyles(); 
    // We have an outer div, possibly with tooltip info, inside which is the displayValue, arrow, and underline, each styled
    // All of the children are absolutely positioned with respect to the outer div
    // The tooltip itself uses the tooltip as a uid an implements a delay
    if (this.props.showTooltip) {
      return (
        <span style={styles.span} onMouseOver={this._onMouseOver}>
          <div style={styles.outer} onClick={this.props.onClick} data-for={this.props.tooltip} data-tip={this.props.tooltip} >
            <div style={styles.label}>
              {this.props.displayValue}
            </div>
            <DropDownArrow style={styles.arrow} />
            <div style={styles.underline}/>
          </div>
          <Tooltip 
            id={this.props.tooltip}
            delayHide={50}
            delayShow={300}
            place="bottom"
            type="info"
            effect="solid"          
            offset={{'top': 10, 'left': 0}} />
        </span>
      );
    } else {
      return (
        <div style={styles.outer} onClick={this.props.onClick} >
          <div style={styles.label}>
            {this.props.displayValue}
          </div>
          <DropDownArrow style={styles.arrow}/>
          <div style={styles.underline}/>
        </div>
      );
    }
  },


});
