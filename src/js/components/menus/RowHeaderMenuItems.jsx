import {logDebug} from '../../AS/Logger';

import React from 'react';

import API from '../../actions/ASApiActionCreators';

export default (row) => [
  {
    primaryText: 'Insert row',
    onclick() {
      logDebug('Insert row', row);
      API.insertRow(row);
    }
  },

  {
    primaryText: 'Delete row',
    onclick() {
      logDebug('Delete row', row);
      API.deleteRow(row);
    }
  }
];
