import React from 'react';
import SampleApp from './SampleApp.jsx';

import injectTapEventPlugin from 'react-tap-event-plugin';
injectTapEventPlugin();

export default React.createClass({

  componentDidMount() {
  },

  componentWillUnmount() {
  },

  render() {
    return (
      <SampleApp />
    );
  }
});
