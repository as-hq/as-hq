import {asMenuBar as zIndex} from '../zIndex';

export default {
  root: {
    width: '100%',
    height: '36px',
    backgroundColor: '#181818',
    zIndex,
  },

  menuRoot: {
    display: 'inline-block',
    zIndex,
  },

  menu: {
    display: 'inline-block',
    minWidth: '80px',
    width: '80px',
    height: '36px',
    textTransform: 'none',
    zIndex,
  },

  menuDropRoot: {
  },

  menuDropItem: {
    lineHeight: '32px',
    fontSize: 15,
    whiteSpace: 'nowrap'
  }
};
