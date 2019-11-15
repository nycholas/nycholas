import { createReducer, createActions, Types as ReduxSauceTypes } from 'reduxsauce';

export const initialState = Object.freeze({
  tracks: [],
  loading: false,
  failed: false,
});

const defaultHandler = (state) => state;

const recommendationsFetchRquestedHandler = (state) => ({
  ...state,
  failed: false,
  loading: true,
});

const recommendationsFetchSuccessHandler = (state, action) => ({
  ...state,
  tracks: action.tracks,
  loading: false,
  failed: false,
});

const recommendationsFetchFailedHandler = (state, message, reason) => ({
  ...state,
  error: { message, reason },
  failed: true,
  loading: false,
});

export const { Types, Creators } = createActions({
  recommendationsFetchRequested: [],
  recommendationsFetchSuccess: ['tracks'],
  recommendationsFetchFailed: ['message', 'reason'],
});

export default createReducer(initialState, {
  [ReduxSauceTypes.DEFAULT]: defaultHandler,
  [Types.RECOMMENDATIONS_FETCH_REQUESTED]: recommendationsFetchRquestedHandler,
  [Types.RECOMMENDATIONS_FETCH_SUCCESS]: recommendationsFetchSuccessHandler,
  [Types.RECOMMENDATIONS_FETCH_FAILED]: recommendationsFetchFailedHandler,
});
