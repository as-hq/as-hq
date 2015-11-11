import {logDebug} from '../../AS/Logger';

import React from 'react';

import API from '../../actions/ASApiActionCreators';

export default (col) => [
  {
    primaryText: 'Insert column',
    onclick() {
      logDebug('Insert column', col);
      API.insertCol(col);
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
