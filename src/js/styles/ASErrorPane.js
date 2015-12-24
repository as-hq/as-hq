/* @flow */

export default {
  root: {
    height: '100%',
    width: '100%'
  },

  showAllContainer: {
    display: 'block',
    position: 'absolute',
    right: 0,
    height: '32px',
    zIndex: 100000
  },

  showAllLabel: {
    display: 'inline-block',
    color: '#ffffff',
    lineHeight: '32px',
    paddingRight: '10px'
  },

  showAllCheckbox: {
    display: 'inline-block',
    marginLeft: '10px',
    marginRight: '20px'
  },

  table: {

  },

  thHeight: '32px',
  th: {
    height: '32px',
    backgroundColor: '#282828'
  },

  widths: ['250px', '150px', undefined],

  thd: {
    height: '32px',
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
