import React from 'react';
import App from './App.jsx';

import injectTapEventPlugin from 'react-tap-event-plugin';
injectTapEventPlugin();

export default React.createClass({

  componentDidMount() {
  },

  componentWillUnmount() {
  },

  render() {
    return (
      <App />
    );
  }
});
