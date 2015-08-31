import React, {PropTypes} from 'react';
import ASRibbonSection from './ASRibbonSection.jsx';
import {AppCanvas, Paper, Styles} from 'material-ui';

let {Colors} = Styles;

export default React.createClass({
  getDefaultProps() {
    return {
      contentsHeight: 110,
      labelHeight: 26
    };
  },

  render() {
    let {sections, labelHeight, contentsHeight} = this.props;

    return (
      <Paper style={{ backgroundColor: '#333333', height: (labelHeight + contentsHeight) + 'px' }}>
        <div style={{ display: 'table-row', position: 'absolute' }}>
          { sections.map((section) => {
            let {label, contents} = section;
            return (
              <ASRibbonSection
                label={label}
                contents={contents}
              />
            );
          }) }
        </div>
        <Paper style={{ backgroundColor: Colors.grey800, height: contentsHeight }}>
        </Paper>
      </Paper>
    );
  }
});
