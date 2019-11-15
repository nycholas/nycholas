import { createReducer, createActions, Types as ReduxSauceTypes } from 'reduxsauce';

export const initialState = Object.freeze({
  artists: [],
  loading: false,
  failed: false,
});

const defaultHandler = (state) => state;

const top5FetchRquestedHandler = (state) => ({
  ...state,
  failed: false,
  loading: true,
});

const top5FetchSuccessHandler = (state, action) => ({
  ...state,
  artists: action.artists,
  loading: false,
  failed: false,
});

const top5FetchFailedHandler = (state, message, reason) => ({
  ...state,
  error: { message, reason },
  failed: true,
  loading: false,
});

export const { Types, Creators } = createActions({
  meTop5FetchRequested: [],
  meTop5FetchSuccess: ['artists'],
  meTop5FetchFailed: ['message', 'reason'],
});

export default createReducer(initialState, {
  [ReduxSauceTypes.DEFAULT]: defaultHandler,
  [Types.ME_TOP5_FETCH_REQUESTED]: top5FetchRquestedHandler,
  [Types.ME_TOP5_FETCH_SUCCESS]: top5FetchSuccessHandler,
  [Types.ME_TOP5_FETCH_FAILED]: top5FetchFailedHandler,
});
