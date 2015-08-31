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
            label: 'Sort and Filter',
            contents:
              <div>
                <ASCol width="50px">
                  <ASCheckedButton height="45px" iconClassName="star" onCheckChange={this.onCheckChange} />
                  <ASCheckedButton height="45px" iconClassName="star" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASCol width="55px">
                  <ASCheckedButton height="90px" label="Sort" iconClassName="sort" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASCol width="55px">
                  <ASCheckedButton height="90px" label="Filter" iconClassName="filter_list" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASCol width="130px">
                  <ASHorizontalDropdownButton label="Clear" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton label="Reapply" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton label="Advanced" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: 'Data Tools',
            contents:
              <div>
                <ASCol width="90px">
                  <ASCheckedButton height="94px" label="Text -> ASCol" iconClassName="text_format" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASCol width="90px">
                  <ASCheckedButton height="94px" label="Duplicates" iconClassName="remove_circle" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASCol width="30px">
                  <ASCheckedButton iconClassName="star" onCheckChange={this.onCheckChange} />
                  <ASCheckedButton iconClassName="star" onCheckChange={this.onCheckChange} />
                  <ASCheckedButton iconClassName="star" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: 'Outline',
            contents:
              <div>
                <ASCol width="110px">
                  <ASHorizontalDropdownButton label="Group" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton label="Ungroup" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton label="Subtotal" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: 'Connectors',
            contents:
              <div>
                <ASCol width="110px">
                  <ASHorizontalDropdownButton label="Import" menuItems={["Excel","SQL","MongoDB"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton label="Query" menuItems={["SQL","MongoDB"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton label="Export" menuItems={["Excel","SQL","MongoDB"]} iconClassName="format_color_reset" />
                </ASCol>
                <ASRibbonDivider />
              </div>
          }
        ]}
      />
    );
  }
});
