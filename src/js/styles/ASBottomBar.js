/* @flow */

export default {
  root: {
    position: 'relative',
    display: 'block',
    width: '100%',
    height: '24px',
    background: '#212121'
  },

  button: {
    position: 'relative',
    display: 'inline-block',
    width: '40px',
    top: '50%',
    transform: 'translateY(-50%)' // vertically center
  },

  tooltip: {
    top: 0,
    zIndex: 1000 // to be visible on top of spreadsheet when closed
  }

  
};
