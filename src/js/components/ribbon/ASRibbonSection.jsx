import React, {PropTypes} from 'react';
import ASRibbonDivider from './ASRibbonDivider.jsx';
import ASRibbonLabel from './ASRibbonLabel.jsx';
import {AppCanvas, Styles} from 'material-ui';

let {Colors} = Styles;

export default React.createClass({
  getDefaultProps() {
    return {
      contentsHeight: 110,
      labelHeight: 26
    };
  },

  render() {
    let {label, contents, labelHeight, contentsHeight} = this.props;

    return (
      <div style={{ display: 'table-cell', verticalAlign: 'top', height: (contentsHeight + labelHeight) + 'px' }}>
        <div style={{ display: 'inline-block', height: contentsHeight + 'px' }}>
          {contents}
        </div>
        <ASRibbonLabel style={{ display: 'block', width: '100%' }} label={label} />
      </div>
    );
  }
});
