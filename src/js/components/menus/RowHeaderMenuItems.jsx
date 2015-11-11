import {logDebug} from '../../AS/Logger';

import React from 'react';

import API from '../../actions/ASApiActionCreators';

export default (row) => [
  {
    primaryText: 'Insert row above',
    onclick() {
      logDebug('Insert row', row);
      API.insertRow(row);
    }
  },

  {
    primaryText: 'Insert row below',
    onclick() {
      logDebug('Insert row', row + 1);
      API.insertRow(row + 1);
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
