/* @flow */

import React from 'react';

import type {
  Callback
} from './Base';

export type RoutedComponentProps = {
  children: ?React.Element;
  history: History;
  location: Location;
  params: Params;
  route: Route;
  routeParams: Params;
  routes: Array<Route>;
}

// ----------------------------------------------------------------------------
// react-router types

export type Action = 'PUSH' | 'REPLACE' | 'POP';
export type Component = ReactClass | string;

// string aliases
type Hash = string;
type Pathname = string;
type QueryString = string;
type RoutePattern = string;
type Path = string; // Pathname + QueryString + Hash

// Hooks
export type EnterHook =
  (nextState: RouterState,
    replace: RedirectFunction,
    callback?: Function)
    => any;
export type LeaveHook = () => any;

// Locations
type Search = any; // there is no documentation on this type

export type Location = {
  pathname: Pathname;
  search: QueryString;
  query: Query;
  state: LocationState;
  action: Action;
  key: LocationKey;
};
export type LocationDescriptorObject = {
  pathname: Pathname;
  search: Search;
  query: Query;
  state: LocationState;
};
export type LocationDescriptor = LocationDescriptorObject | Path;
export type LocationKey = string;
export type LocationState = ?Object;

// paths & routes
type RouteComponent = Component;
type RouteConfig = Array<Route>;

export type Query = Object;
export type Params = Object;
export type RedirectFunction =
  (state: ?LocationState,
    pathname: Pathname | Path,
    query: ?Query)
    => void;
export type Route = {
  component: RouteComponent;
  path: ?RoutePattern;
  onEnter: ?EnterHook;
  onLeave: ?LeaveHook;
};
export type RouteHook = (nextLocation?: Location) => any;

// Router
export type Router = {
  push: (location: LocationDescriptor) => void;
  replace: (location: LocationDescriptor) => void;
  go: (n: number) => void;
  goBack: () => void;
  goForward: () => void;
  setRouteLeaveHook: (hook: RouteHook) => Function;
  isActive: (location: LocationDescriptor, indexOnly: boolean) => void;
};
export type RouterState = {
  location: Location;
  routes: Array<Route>;
  params: Params;
  components: Array<Component>;
};

// History
export type History = {
  goBack: () => void;
  goForward: () => void;
  match: (location: Location, cb: Callback) => void;
  push: (location: Location | Pathname) => void;
  pushState: (state: LocationState, path: Path, query: Query) => void;
  replace: (location: Location | Pathname) => void;
  replaceState: (state: LocationState, path: Path, query: Query) => void;
};
