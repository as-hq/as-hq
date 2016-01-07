/* @flow */

export default {
  root: {
    display: 'block',
    width: '100%',
    height: '24px',
    lineHeight: '24px',
    background: '#212121'
  },

  errorsButton: {
    marginLeft: '-20px',
    lineHeight: '24px',
    paddingTop: '0px',
    paddingLeft: '0px',
    paddingRight: '0px',
    // this is a hack, to get the clickable area of errorsButton
    // visible at all. Because in both buttons, the clickable area is
    // huge due the icon misaligning itself with the clickable area (?!),
    // which causes CSS to think the width of the entire button div is equal
    // to the width of the button PLUS the amount of misaligment, producing a
    // div of 126px instead of 24px.
    zIndex: 5
  },

  // NOTE on shitty margin-top CSS:
  // somehow, the underlying fonticon distances itself from the clickable area???!
  // what the fuck??!!
  // #ui-guy
  errorsIcon: {
    marginLeft: '-85px',
    fontSize: '18px',
    paddingTop: '0px',
    paddingLeft: '0px',
    paddingRight: '0px'
  },

  outputButton: {
    lineHeight: '24px',
    paddingTop: '0px',
    paddingLeft: '0px',
    width: '24px',
    paddingRight: '0px'
  },

  outputIcon: {
    marginLeft: '-130px',
    fontSize: '18px',
    paddingTop: '0px',
    paddingLeft: '0px',
    paddingRight: '0px'
  }
};
