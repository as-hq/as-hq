/* @flow */

import type {
  RoutedComponentProps
} from '../types/Router';

import type {
  StoreToken
} from 'flux';

import React from 'react';
import Constants from '../Constants';
// $FlowFixMe
import { RouteHandler } from 'react-router';

import API from '../actions/ASApiActionCreators';
import LoginActions from '../actions/ASLoginActionCreators';
import LoginStore from '../stores/ASLoginStore';

type LoginProps = RoutedComponentProps;

class Login extends React.Component<{}, LoginProps, {}> {
  _storeToken: StoreToken;

  componentDidMount() {
    // $FlowFixMe gapi is in scope due to a top-level script import in Index.html
    gapi.signin2.render('google-signin-button', {
      onsuccess: this._onLoginSubmit,
      ...properties.googleSignin
    });
    this._storeToken = LoginStore.addListener(() => this._onLoginStateChange());
  }

  componentWillUnmount() {
    this._storeToken.remove();
  }

  render(): React.Element {
    return (
      <div style={styles.root}>
        <div style={styles.loginContainer}>
          <h1 style={styles.title}>Login</h1>
          <meta name="google-signin-client_id" content={Constants.google_client_id} />
          <div id='google-signin-button' style={styles.googleSignin}/>
        </div>
      </div>
    );
  }

  _onLoginSubmit(user: any) {
    const idToken = user.getAuthResponse().id_token;
    LoginActions.login(idToken);
  }

  _onLoginStateChange() {
    if (LoginStore.isLoggedIn()) {
      // This redirection needs to be asynchronous, else every component that
      // gets mounted as a result cannot fire action creators upon mount.
      //
      // onLoginSuccess --> Dispatch -//-> redirect --> componentDidMount ASEvalPane --> Dispatch
      //                              ^
      //                              break synchronicity
      setTimeout(() => {
        this.props.history.push('/app');
      }, 0);
    }
  }
}

const properties = {
  googleSignin: {
    scope: 'https://www.googleapis.com/auth/plus.login',
    width: 250,
    height: 50,
    longtitle: true,
  }
}

const styles = {
  root: {
    height: '100%',
    display: 'flex',
    justifyContent: 'center',
    background: `
      linear-gradient(
        rgba(0,0,0,0.8),
        rgba(0,0,0,0.8)
      ),
      url(http://static1.squarespace.com/static/567256080e4c11f307ac07ef/t/5678fbcc05f8e228a3996837/1450769356724/download.jpeg)
      `,
  },
  loginContainer: {
    position: 'relative',
    width: 350,
    height: 500,
    borderRadius: '3px',
    boxShadow: '0px 0px 20px 4px #000000',
    backgroundColor: '#fff',
    alignSelf: 'center',
  },
  title: {
    position: 'absolute',
    top: 20,
    left: '30%',
    fontFamily: 'Code',
    textTransform: 'uppercase'
  },
  googleSignin: {
    position: 'absolute',
    left: 'calc(50% - 125px)',
    bottom: 20
  }
}

export default Login;