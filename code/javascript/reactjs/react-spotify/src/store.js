import createSagaMiddleware from 'redux-saga';
import { createStore, applyMiddleware, compose } from 'redux';

import { reducers, rootSagas } from './services';

let composeEnhancers = compose;
if (['development', 'dev'].includes(process.env.NODE_ENV.trim())) {
  composeEnhancers = window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ || compose;
}

const sagaMiddleware = createSagaMiddleware();

export default createStore(reducers, composeEnhancers(applyMiddleware(sagaMiddleware)));

sagaMiddleware.run(rootSagas);
