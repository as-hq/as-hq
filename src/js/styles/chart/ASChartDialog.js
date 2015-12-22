/* @flow */

export default {
  inputs: {
    width: '200px',
    paddingLeft: '0px'
  },

  toggles: {
    fontSize: '16px'
  },

  divider: {
    root: { display: 'table-cell' },
    inner: { display: 'block', width: '40px' }
  },

  settingsPanel: {
    display: 'table-cell',
    width: '250px',
    verticalAlign: 'top'
  },

  previewPanel: {
    display: 'table-cell',
    width: '100%',
    verticalAlign: 'top'
  }
};
