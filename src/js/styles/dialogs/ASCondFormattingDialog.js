/* @flow */

import type {
  Dict
} from '../../types/Base';

export default {
  ruleContainer(idx: number): Dict<any> {
    let backgroundColor =
      (idx % 2 === 0)
        ? '#303030'
        : '#424242'
    return ({
      paddingLeft: '16px',
      paddingRight: '16px',
      backgroundColor: backgroundColor
    });
  },

  title: {
    display: 'inline-block',
    color: '#eeeeee',
    lineHeight: '20px',
    paddingTop: '0px',
    marginTop: '18px',
    marginBottom: '18px',

    textOverflow: 'ellipsis',
    whiteSpace: 'nowrap',
    overflow: 'hidden',
    paddingRight: '20px',
    width: '500px'
  },

  buttons: {
    float: 'right',
    marginTop: '10px',
    marginBottom: '10px'
  },

  formDropdowns: {
    width: '400px'
  },

  formTextFields: {
    width: '400px',
    paddingLeft: '24px'
  },

  formColorPicker: {
    marginLeft: '24px'
  }
}
