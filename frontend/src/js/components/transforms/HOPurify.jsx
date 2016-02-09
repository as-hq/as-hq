/* @flow */

import type {
  Callback,
  Dict
} from '../../types/Base';

import React from 'react';

import Util from '../../AS/Util';

import _ from 'lodash';

export default function HOPurify<T: ReactClass, U>({
  component: Component, purifiers
}: {
  component: T;
  purifiers: Array<{
    name: string;
    addChangeListener: (component: React.Element, listener: (val: U) => void) => void;
    removeChangeListener: (component: React.Element, listener: (val: U) => void) => void;
    getValue: (component: React.Element) => U;
    setValue: (component: React.Element, val: U) => void;
  }>;
}): ReactClass {
  const names = purifiers.map(({name}) => name);

  // This class will have props whose names correspond to purifiers.map((p) => p.name)
  return React.createClass({
    $listenerRemovers: ([]: Array<Callback>),

    purifiers: ({}: Dict<{ getValue: () => U; setValue: (val: U) => void; }>),
    silent: ({}: Dict<boolean>),

    _getInstance(): React.Element { return this.refs.instance; },

    _getPropsValue(name: string): U {
      return this.props[name].value;
    },

    _resetValue(name: string) {
      this._silentlyFor(name, () => {
        this.purifiers[name].setValue(this._getPropsValue(name));
      });
    },

    _propsRequestChange(name: string, val: U) {
      this.props[name].requestChange(val);
    },

    _reportChange(name: string, val: U) {
      if (!this.silent[name]) {
        const propLens = this.purifiers[name];
        const newVal = propLens.getValue();
        this._resetValue(name);
        this._propsRequestChange(name, newVal);
      }
    },

    _silentlyFor(name: string, cb: Callback) {
      this.silent[name] = true;
      cb();
      this.silent[name] = false;
    },

    componentDidMount() {
      const _this = this;

      purifiers.forEach(({name, getValue, setValue}) => {
        _this.purifiers[name] = ({
          getValue() { return getValue(_this._getInstance()); },
          // NOTE: setValue should include things like fixing cursor position while setting
          setValue(val: U) { return setValue(_this._getInstance(), val); }
        });
      });

      const listeners = purifiers.map(
        ({name, addChangeListener, removeChangeListener}) => ({
          listener: (val) => { _this._reportChange(name, val); },
          add(l) { addChangeListener(this._getInstance(), l); },
          remove(l) { removeChangeListener(this._getInstance(), l); }
        })
      );

      Util.React.implementComponentListeners(this, listeners);
    },

    componentWillUnmount() {
      Util.React.removeComponentListeners(this);
    },

    componentWillReceiveProps(nextProps: Dict<ReactLink>) {
      names.forEach((name) => {
        const {value, requestChange} = nextProps[name];
        this._silentlyFor(name, () => {
          const propLens = this.purifiers[name];
          if (propLens.getValue() !== value) {
            propLens.setValue(value);
          }
        });
      });
    },

    render(): React.Element {
      return (
        <Component
          ref="instance"
        />
      );
    }
  });
}
