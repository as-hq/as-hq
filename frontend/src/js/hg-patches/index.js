/* @flow */

import type {Callback} from '../types/Base';
import type ASSpreadsheet from '../components/ASSpreadsheet.jsx';

import Behavior from './Behavior';
import EventHandlers from './EventHandlers';
import Renderers from './Renderers';

import _ from 'lodash';

export default (_.flatten([Behavior, EventHandlers, Renderers]): Array<Callback<ASSpreadsheet>>);
