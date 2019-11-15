import { createReducer, createActions, Types as ReduxSauceTypes } from 'reduxsauce';

export const initialState = Object.freeze({
  albums: [],
  loading: false,
  failed: false,
});

const defaultHandler = (state) => state;

const newReleasesFetchRquestedHandler = (state) => ({
  ...state,
  failed: false,
  loading: true,
});

const newReleasesFetchSuccessHandler = (state, action) => ({
  ...state,
  albums: action.albums,
  loading: false,
  failed: false,
});

const newReleasesFetchFailedHandler = (state, message, reason) => ({
  ...state,
  error: { message, reason },
  failed: true,
  loading: false,
});

export const { Types, Creators } = createActions({
  browseNewReleasesFetchRequested: [],
  browseNewReleasesFetchSuccess: ['albums'],
  browseNewReleasesFetchFailed: ['message', 'reason'],
});

export default createReducer(initialState, {
  [ReduxSauceTypes.DEFAULT]: defaultHandler,
  [Types.BROWSE_NEW_RELEASES_FETCH_REQUESTED]: newReleasesFetchRquestedHandler,
  [Types.BROWSE_NEW_RELEASES_FETCH_SUCCESS]: newReleasesFetchSuccessHandler,
  [Types.BROWSE_NEW_RELEASES_FETCH_FAILED]: newReleasesFetchFailedHandler,
});
