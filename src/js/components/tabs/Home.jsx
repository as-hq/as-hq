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
  ASDropdown,
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
            label: "Clipboard",
            contents:
              <div>
                <ASCol width="70px">
                  <ASBlockDropdownButton height="94px" label="Paste" menuItems={["paste"]} iconClassName="home" />
                </ASCol>
                <ASCol width="30px">
                  <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: "Font",
            contents:
              <div>
                <ASCol width="150px">
                  <ASDropdown
                    menuItems={[
                      { payload: 'key1', text: 'Test' }
                    ]}
                    style={{
                      width: '140px'
                    }}
                  />
                  <ASRow width="150px" >
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                </ASCol>
                <ASCol width="80px">
                  <ASDropdown
                    menuItems={[
                      { payload: 'key1', text: 'Test' }
                    ]}
                    style={{
                      width: '140px'
                    }}
                  />
                  <ASRow width="70px" >
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                </ASCol>
                <ASCol width="70px">
                  <ASRow width="70px">
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                  <ASRow width="70px" >
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: "Alignment",
            contents:
              <div>
                <ASCol width="140px">
                  <ASRow width="140px" >
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                  <ASRow width="140px" >
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                   <ASRow width="140px" >
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: "Number",
            contents:
              <div>
                <ASCol width="140px">
                  <ASRow width="140px" >
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                  <ASRow width="140px" >
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                   <ASRow width="140px" >
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                    <ASCheckedButton iconClassName="home" onCheckChange={this.onCheckChange} />
                  </ASRow>
                </ASCol>
                <ASRibbonDivider />
              </div>
          },
          {
            label: "Styles",
            contents:
              <div>
                <ASCol width="230px">
                  <ASHorizontalDropdownButton width="230px" height="30px" label="Conditional Formatting" menuItems={["Alpha","Sheets"]} iconClassName="format_color_reset" />
                  <ASHorizontalDropdownButton width="230px" height="30px" label="Format as Table" menuItems={["Format"]} iconClassName="grid_on" />
                  <ASHorizontalDropdownButton width="230px" height="30px" label="Cell Styles" menuItems={["Alpha","Sheets"]} iconClassName="brush" />
                </ASCol>
                <ASRibbonDivider />
              </div>
          }
        ]}
      />
    );
  }
});
