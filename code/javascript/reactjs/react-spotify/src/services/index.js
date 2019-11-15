import { combineReducers } from 'redux';
import { all } from 'redux-saga/effects';

import { globalReducers } from './global';
import { meReducers, meSagas } from './me';
import { browseReducers, browseSagas } from './browse';
import { recommendationReducers, recommendationSagas } from './recommendation';

export const reducers = combineReducers({
  globalReducers,
  meReducers,
  browseReducers,
  recommendationReducers,
});

export function* rootSagas() {
  yield all([...meSagas, ...browseSagas, ...recommendationSagas]);
}
