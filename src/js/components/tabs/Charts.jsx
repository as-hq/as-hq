import React, {PropTypes} from 'react';

import {Styles, FontIcon, DropDownMenu} from 'material-ui'

import {
  ASRibbonTabContents,
  ASRibbonSection,
  ASRibbonDivider,
  ASRow,
  ASCol
} from '../ribbon/index.jsx';

import {
  ASButton,
  ASCheckedButton,
  ASDropdownButton,
  ASBlockDropdownButton,
  ASHorizontalDropdownButton
} from '../basic-controls/index.jsx';

export default React.createClass({
  componentDidMount() {

  },

  render() {
    return (
      <ASRibbonTabContents
        sections={[
          {
            label: "Tables",
            contents:
              <div>
                <ASCol width="60px">
                    <ASBlockDropdownButton height="90px" label="Pivot" menuItems={["Pivot","Table"]} iconClassName="flip_to_back" />
                </ASCol>
                <ASCol width="60px">
                  <ASCheckedButton height="90px" label="Table" iconClassName="star" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: "Illustrations",
            contents:
              <div>
                <ASCol width="70px">
                  <ASCheckedButton height="90px" label="Picture" iconClassName="star" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASCol width="70px">
                  <ASCheckedButton height="90px" label="Clip Art" iconClassName="star" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: "Charts",
            contents:
              <div>
                <ASCol width="90px">
                  <ASBlockDropdownButton width="80px" height="90px" label="ASColumns" menuItems={["ASColumn","Shit"]} iconClassName="flip_to_back" />
                </ASCol>
                <ASCol width="110px">
                  <ASHorizontalDropdownButton width="100px" height="30px" label="Line" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton width="100px" height="30px" label="Pie" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton width="100px" height="30px" label="Bar" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                </ASCol>
                <ASCol width="150px">
                  <ASHorizontalDropdownButton width="150px" height="30px" label="Area" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton width="150px" height="30px" label="Scatter" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton width="150px" height="30px" label="Other Charts" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: "Hyperlinks",
            contents:
              <div>
                <ASCol width="90px">
                  <ASCheckedButton height="90px" label="Hyperlink" iconClassName="check_circle" />
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: "Symbols",
            contents:
              <div>
                <ASCol width="90px">
                  <ASBlockDropdownButton width="80px" height="90px" label="Symbols" menuItems={["ASColumn","Shit"]} iconClassName="details" />
                </ASCol>
                <ASRibbonDivider />
              </div>
          }
        ]}
      />
    );
  }
});
