import {logDebug} from '../../AS/Logger';

import React from 'react';

import API from '../../actions/ASApiActionCreators';

export default (col) => [
  {
    primaryText: 'Insert column to left',
    onclick() {
      logDebug('Insert column', col);
      API.insertCol(col);
    }
  },

  {
    primaryText: 'Insert column to right',
    onclick() {
      logDebug('Insert column', col + 1);
      API.insertCol(col + 1);
    }
  },

  {
    primaryText: 'Delete column',
    onclick() {
      logDebug('Delete column', col);
      API.deleteCol(col);
    }
  }
];
