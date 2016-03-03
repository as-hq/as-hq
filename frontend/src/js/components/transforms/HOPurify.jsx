/* @flow */

import type {
  Callback,
  Dict
} from '../../types/Base';

import React from 'react';

import Util from '../../AS/Util';

import _ from 'lodash';

/*
  This function takes a component with stateful behavior for some of its
  properties, and returns a component with pure props corresponding to each of
  those properties.

  For example, look at ASCodeField. It has:

      - a selection that it modifies by itself, not through a prop
      - the ability to listen to change events on the selection
      - the ability to get the value of the current selection
      - the ability to move the selection to some range

  In theory, this is enough to create ASCursorControlledCodeField, which:

      - has a selection prop with:
        - a value, which is the desired value of the selection
        - a requestChange(a, b) method, which sets the state of the component to
          a and calls b() when it's done (b is passed into setState)

      - when the value prop is updated, ASCCCF will update the selection

      - the requestChange() prop is called whenever ASCF wants to update the
        selection, but

      - ASCF doesn't immediately update it and waits for permission from the
        parent component

  In practice, this is a bunch of boilerplate that is difficult to debug and
  must be debugged over and over again every time a purification occurs.

  We are making HOPurify to simplify this process.
*/


type PropChangeAction = (reporter: Callback) => void;

type Locker = {
  lock: (name: string) => void;
  unlock: (name: string) => void;
}

type Purifiers = Dict<{
  addChangeListener: (arg: {
    component: any;
    listener: Callback;
    locker: Locker;
  }) => void;
  getValue: (component: React.Element) => any;
  setValue: (arg: {
    component: any;
    value: any;
  }) => void;
  removeChangeListener?: (component: React.Element, listener: Callback) => void;
  manuallySilenced?: boolean;
}>;

// this type annotation exists only to clarify the structure of the prop
// that should be passed in for a purified underlying prop
type PurifiedProps = Dict<{
  value: any;
  requestChange: (val: any) => void;
} | any>;

type args = {
  component: ReactClass;
  purifiers: Purifiers;
  onReady?: (cb: Callback) => void;
// ^ a callback in componentDidMount to wait for components which initialize asynchronously (e.g. hypergrid)
};

export default function HOPurify({component: Component, purifiers, onReady}: args): ReactClass {
  const names = Object.keys(purifiers);

  return class PurifiedComponent extends React.Component<{}, PurifiedProps, {}> {
    $listenerRemovers: Array<Callback>;
    _instance: ReactElement;
    _silent: Dict<boolean>;
    // for listeners that depend on each others' behavior.
    // this is essential for Ace to function properly.
    _locked: Dict<boolean>;
    _locker: Locker;

    constructor(props: PurifiedProps) {
      super(props);
      this.$listenerRemovers = [];
      this._silent = {};
      this._locked = {};
      this._locker = {
        lock: (name) => { this._locked[name] = true; },
        unlock: (name) => { this._locked[name] = false; }
      }
    }

    getInstance(): ReactElement {
      return this._instance;
    }

    componentDidMount() {
      if (!! onReady) {
        onReady(() => this._onMount());
      } else {
        this._onMount();
      }
    }

    componentWillUnmount() {
      Util.React.removeComponentListeners(this);
    }

    componentWillReceiveProps(nextProps: Dict<ReactLink>) {
      names.forEach((name) => {
        const {value} = nextProps[name];
        this._setUnderlyingValue(name, value);
      });
    }

    render(): React.Element {
      // determine props to be passed to child
      let otherProps = {};
      _.forEach(this.props, (val, key) => {
        if (!names.includes(key)) {
          otherProps[key] = val;
        }
      });

      return (
        <Component
          ref={elem => this._instance = elem}
          {...otherProps} />
      );
    }

    _getPropsValue(name: string): any {
      return this.props[name].value;
    }

    _getUnderlyingValue(name: string): any {
      return purifiers[name].getValue(this._instance);
    }

    _setUnderlyingValue(name: string, targetValue: any) {
      if (! this._locked[name]) {

        // will perform 'silently' unless the flag 'manuallySilenced' is true
        this._silently(name, () => {
          const currentValue = this._getUnderlyingValue(name)

          if (!_.isEqual(currentValue, targetValue)) {
            purifiers[name].setValue({
              component: this._instance,
              value: targetValue,
            });
          }
        });
      }
    }

    _propsRequestChange(name: string, val: any, metadata: any, cb: Callback) {
      this.props[name].requestChange(val, metadata);
      cb();
    }

    _resetValue(name: string) {
      const val = this._getPropsValue(name);
      this._setUnderlyingValue(name, this._getPropsValue(name));
    }

    _silently(name: string, cb: Callback) {
      if (! purifiers[name].manuallySilenced) {
        this._silent[name] = true;
      }
      cb();
      this._silent[name] = false;
    }

    _reportChange(name: string, val: any, metadata: any) {
      if (! this._silent[name]) {
        this._propsRequestChange(name, val, metadata, () => {
          this._resetValue(name);
        });
      }
    }

    _onMount() {
      const _this = this;
      const listeners = _.map(purifiers,
        ({addChangeListener, removeChangeListener}, name) => ({

          listener: (metadata) => {
            const currentValue = _this._getUnderlyingValue(name);
            _this._reportChange(name, currentValue, metadata || null);
          },

          add(l) {
            addChangeListener({
              component: _this._instance,
              listener: l,
              locker: _this._locker
            });
          },

          remove(l) {
            if (removeChangeListener) {
              removeChangeListener({
                component: _this._instance,
                listener: l
              });
            }
          }
        })
      );

      Util.React.implementComponentListeners(this, listeners);

      // component doesn't receive props upon mount, so we need to init
      names.forEach((name) => {
        this._setUnderlyingValue(name, this._getPropsValue(name));
      });
    }
  }
}
