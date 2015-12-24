/* @flow */

export default {
  root: {
    height: '100%',
    width: '100%'
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
