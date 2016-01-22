/* @flow */

/* NOTE:

There has been a problem in AlphaSheets code where views have too much state
that simply comes from stores. The point of this file is to ensure that we can
just do:

    componentDidMount() {
      Util.React.addStoreLinks(this, [
        { store: SelectionStore }
      ]);
    }

    componentWillUnmount() {
      Util.React.removeStoreLinks(this);
    }

and then be able to forceUpdate on render,

 */

// TODO: refactor everything into $destructors

import type {
  Callback,
  Dict
} from '../../types/Base';

import type {
  StoreLink
} from '../../types/React';

import React from 'react';

import BaseStore from '../../stores/BaseStore';

type StoreLinkedReactComponent = ReactComponent & {
  $storeLinks: Array<StoreLink>;
};

type ListenedReactComponent = ReactComponent & {
  $listenerRemovers: Array<Callback>;
};

const ASReactUtils = {
  addStoreLinks(
    component: StoreLinkedReactComponent,
    storeLinks: Array<{
      listener?: Callback;
      store: typeof BaseStore;
    }>
  ) {
    component.$storeLinks = [];
    storeLinks.forEach(({listener, store}) => {
      const fn = () => {
        component.forceUpdate();
        if (listener) { listener(); }
      };
      component.$storeLinks.push({
        store, listener: fn
      });
      store.addChangeListener(fn);
    });
  },

  removeStoreLinks(component: StoreLinkedReactComponent) {
    component.$storeLinks.forEach(({store, listener}) => {
      store.removeChangeListener(listener);
    });
    component.$storeLinks = [];
  },

  implementComponentListeners(
    component: ListenedReactComponent,
    listenerImplementers: Array<{
      listener: Callback;
      add: (listener: Callback) => void;
      remove: (listener: Callback) => void;
    }>
  ) {
    listenerImplementers.forEach(({listener, add, remove}) => {
      // add the listeners
      add(listener);

      // store listeners for later removal
      component.$listenerRemovers.push(() => remove(listener));
    });

  },

  removeComponentListeners(component: ListenedReactComponent) {
    component.$listenerRemovers.forEach((f) => f());
  }
};

export default ASReactUtils;
