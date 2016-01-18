/* @flow */

export default {
  root: {
    height: '100%',
    width: '100%'
  },

  topBar: {
    height: '26px',
    width: '100%',
    background: '#212121'
  },

  topBarTitle: {
    color: '#f8f8f2',
    fontSize: '12',
    lineHeight: '26px',
    fontWeight: 'bold',
    position: 'inline',
    paddingLeft: '10px'
  },

  contentPane: {
    height: 'calc(100% - 26px)',
    width: '100%',
    overflow: 'auto'
  },

  outputLine: {
    lineHeight: '14px',
    fontFamily: 'monospace',
    color: '#f8f8f2' // as the default un-formatted text color
  },

  contentContainer: {
    paddingTop: '5px',
    paddingLeft: '10px'
  },

  altMessage: {
    color: 'grey'
  }

}
