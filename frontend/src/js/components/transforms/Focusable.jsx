/* @flow */

import type { FocusedElement } from '../../types/State';
import type { StoreToken } from 'flux';
import type {
  Callback,
  Dict
} from '../../types/Base';

import React from 'react';

import FocusStore from '../../stores/ASFocusStore';
import FocusActions from '../../actions/ASFocusActionCreators';

type Focuser = {
  name: FocusedElement;
  async?: boolean;
  takeFocus: (component: any) => void;
  addFocusListener?: (component: any, listener: Callback) => void;
};

export default function Focusable(Component: ReactClass, focuser: Focuser): ReactClass {
  const { name, takeFocus, addFocusListener } = focuser;

  return class FocusedComponent extends React.Component<{}, any, {}> {
    _component: any;
    _storeToken: StoreToken;
    _silent: boolean;
    _listener: Callback;
    _additionalProps: Dict<any>;

    constructor(props) {
      super(props);
      this._silent = false;
      this._listener = () => {
        if (! this._silent) {
          FocusActions.focus(name);
        }
      };
      this._additionalProps = {};
    }

    componentDidMount() {
      this._storeToken = FocusStore.addListener(() =>
        this._onFocusChange()
      );

      if (!! addFocusListener) {
        addFocusListener(this._component, this._listener);
      } else {
        // if no listener method provided, use the default react onFocus.
        this._additionalProps['onFocus'] = this._listener;
      }
    }

    componentWillUnmount() {
      this._storeToken.remove();
    }

    render() {
      return (<Component
                ref={elem => this._component = elem}
                {...this.props}
                {...this._additionalProps}
              />);
    }

   /**
    * Called when FocusStore emits; synchronizes focus with the store
    * by taking focus if necessary.
    */
    _onFocusChange() {
      if (FocusStore.isFocused(name)) {
        if (focuser.async) {
          setTimeout(() => this._takeFocus(), 10);
        } else {
          this._takeFocus();
        }
      }
    }

    _takeFocus() {
      this._silent = true;
      takeFocus(this._component);
      this._silent = false;
    }
  }
}
