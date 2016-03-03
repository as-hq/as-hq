/* @flow */

import React from 'react';
import {Styles} from 'material-ui';
// $FlowFixMe
import pure from 'recompose/pure';

// $FlowFixMe
import DropDownArrow from 'material-ui/lib/svg-icons/navigation/arrow-drop-down';

import Tooltip from 'react-tooltip';


type ToolbarTextFieldDefaultProps = {
  width: number;
  height: number;
  spacing: number;
  onClick: (e: SyntheticMouseEvent) => void;
  showTooltip: boolean;
  arrowSize: number;
};

type ToolbarTextFieldProps = {
  displayValue: string;
  onClick: (e: SyntheticMouseEvent) => void;
  width: number;
  height: number;
  spacing: number;
  tooltip: string;
  showTooltip: boolean;
  arrowSize: number;
};

function getStyles(props: ToolbarTextFieldProps): any {
  const {width, height, spacing, arrowSize} = props;

  return {
    outer: {
      display: 'inline-block', // should stack horizontally with other toolbar components
      position: 'relative',
      top: '50%',
      transform: 'translateY(-50%)',
      width,
      height,
      cursor: 'pointer',
      marginLeft: spacing,
    },
    arrow: { // arrow should be towards the right
      position: 'absolute',
      fill: Styles.Colors.grey50,
      width: arrowSize,
      right: '10%',
      top: '25%',
      marginTop: 0,
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
}

// Return a component with underline, arrow, and text, and a tooltip if
// applicable.
//
// TODO: There's some redundancy that can probably be eliminated with React
// helpers.
function ToolbarTextField(props: ToolbarTextFieldProps): React.Element {
  const styles = getStyles(props);
  const {onClick, tooltip, displayValue, showTooltip} = props;

  // We have an outer div, possibly with tooltip info, inside which is the
  // displayValue, arrow, and underline, each styled
  //
  // All of the children are absolutely positioned with respect to the outer
  // div
  //
  // The tooltip itself uses the tooltip as a uid an implements a delay

  const inner = (
    <div
      style={styles.outer}
      onClick={onClick}
      data-for={tooltip}
      data-tip={tooltip}
    >
      <div style={styles.label}>
        {displayValue}
      </div>
      <DropDownArrow style={styles.arrow} />
      <div style={styles.underline} />
    </div>
  );

  const tip = (
    <Tooltip
      id={tooltip}
      delayHide={50}
      delayShow={300}
      place="bottom"
      type="info"
      effect="solid"
      offset={{'top': 10, 'left': 0}}
    />
  );

  return (
    <span>
      {inner}
      {showTooltip && tip}
    </span>
  );
}

ToolbarTextField.propTypes = {
  // textfield value
  displayValue: React.PropTypes.string.isRequired,
  onClick: React.PropTypes.func,
  width: React.PropTypes.number,
  height: React.PropTypes.number,
  // left margin
  spacing: React.PropTypes.number,
  tooltip: React.PropTypes.string,
  showTooltip: React.PropTypes.bool,
  arrowSize: React.PropTypes.number,
};

ToolbarTextField.defaultProps = {
  width: 100,
  height: 36,
  spacing: 0, // most of the time there's a separator right before this element, so no spacing required
  onClick: () => {},
  showTooltip: true,
  arrowSize: 15
};

export default pure(ToolbarTextField);
