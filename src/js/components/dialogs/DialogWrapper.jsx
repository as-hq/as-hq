import React, {PropTypes} from 'react';

import {Dialog} from 'material-ui';

export default React.createClass({
  getDefaultProps() {
    return ({
      open: false
    });
  },

  componentWillReceiveProps(nextProps) {
    if (! this.props.open && nextProps.open) {
      this.refs.dialog.show();
    } else if (this.props.open && ! nextProps.open) {
      this.refs.dialog.dismiss();
    }
  },

  render() {
    let {title, actions, onRequestClose} = this.props;

    return (
      <Dialog
        title={title}
        actions={actions}
        onClickAway={onRequestClose}
        onDismiss={onRequestClose}
        ref="dialog">
        {this.props.children}
      </Dialog>
    );
  }
});
