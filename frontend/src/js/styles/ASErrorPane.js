/* @flow */
import {asErrorPane as zIndex} from './zIndex';

export default {
  root: {
    height: '100%',
    width: '100%'
  },

  showAllContainer: {
    display: 'block',
    position: 'absolute',
    right: 0,
    height: '26px',
    zIndex,
  },

  showAllLabel: {
    display: 'inline-block',
    color: '#ffffff',
    lineHeight: '26x',
    paddingRight: '10px'
  },

  showAllCheckbox: {
    display: 'inline-block',
    marginLeft: '10px',
    marginRight: '20px'
  },

  table: {

  },

  thHeight: '26px',
  th: {
    height: '26px',
    backgroundColor: '#282828'
  },

  widths: ['250px', '150px', undefined],

  thd: {
    height: '26px',
    color: '#ffffff',
    fontSize: '10pt',
    fontWeight: '500'
  },

  tbody: {

  },

  tr(idx: number): {[key: string]: any} {
    return {
      height: '24px',
      backgroundColor: (idx % 2 === 0) ? '#424242' : '#333333'
    };
  },

  td: {
    fontSize: '18px'
  }
};
