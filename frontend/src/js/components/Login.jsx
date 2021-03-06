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
// $FlowFixMe
import CircularProgress from 'material-ui/lib/circular-progress';

import API from '../actions/ASApiActionCreators';
import LoginActions from '../actions/ASLoginActionCreators';
import NotificationActions from '../actions/ASNotificationActionCreators';
import LoginStore from '../stores/ASLoginStore';
import ConfigStore from '../stores/ASConfigurationStore';

import pws from '../AS/PWSInstance';

type Props = RoutedComponentProps;
type State = {
  isLoggingIn: boolean;
}

class Login extends React.Component {
  static defaultProps: {} = {};
  props: Props;
  state: State;
  _storeToken: StoreToken;

  constructor(props: Props) {
    super(props);
    this.state = {
      isLoggingIn: false
    };
  }

  componentDidMount() {
    this._storeToken = LoginStore.addListener(() => this._onLoginStateChange());

    if (LoginStore.isPublicLogin()) {
      console.error('PUBLIC LOGIN!!!');
      this.setState({isLoggingIn: true});
      API.loginPublicly();
    } else {
      // if not public login, render the usual Google signin button.
      // $FlowFixMe gapi is in scope due to a top-level script import in Index.html
      gapi.signin2.render('google-signin-button', {
        onsuccess: (user) => this._onLoginSubmit(user),
        ...properties.googleSignin
      });
    }

  }

  componentWillUnmount() {
    this._storeToken.remove();
  }

  render(): React.Element {
    const {isLoggingIn} = this.state;

    return (
      <div style={styles.root}>
        <div style={styles.loginContainer}>
          <h1 style={styles.title}>Login</h1>
          { isLoggingIn &&
            <CircularProgress style={styles.loader} size={1.5} color='grey' />
          }
          <meta name='google-signin-client_id' content={Constants.google_client_id} />
          <div id='google-signin-button' style={styles.googleSignin}/>
        </div>
      </div>
    );
  }

  _onLoginSubmit(user: any) {
    this.setState({isLoggingIn: true});

    pws.whenReady(() => {
      const { id_token } = user.getAuthResponse();
      LoginActions.login(id_token);
    });

    // Warn user if login submit appears to take more than 5 seconds.
    setTimeout(() => {
      if (! LoginStore.isLoggedIn() && ConfigStore.isConnected()) {
        NotificationActions.addNotification({
          title: 'The server appears to be taking a long time to respond.',
          message: 'Try refreshing the page?',
          level: 'info',
          position: 'bc'
        });
      }
    }, 5000);
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
    } else {
      // Try logging in again upon failure.
      LoginActions.relogin();
    }
  }
}

const properties = {
  googleSignin: {
    scope: 'https://www.googleapis.com/auth/userinfo.profile',
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
  loader: {
    position: 'absolute',
    top: 250,
    left: '35%'
  },
  googleSignin: {
    position: 'absolute',
    left: 'calc(50% - 125px)',
    bottom: 20
  }
}

export default Login;
