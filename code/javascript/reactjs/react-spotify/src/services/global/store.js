import { createReducer, createActions, Types as ReduxSauceTypes } from 'reduxsauce';

export const initialState = Object.freeze({
  breadcrumbs: [],
});

const defaultHandler = (state) => state;

const screenDidMountHandler = (state, action) => ({
  ...state,
  breadcrumbs: action.breadcrumbs,
});

export const { Types, Creators } = createActions({
  globalScreenDidMount: ['breadcrumbs'],
});

export default createReducer(initialState, {
  [ReduxSauceTypes.DEFAULT]: defaultHandler,
  [Types.GLOBAL_SCREEN_DID_MOUNT]: screenDidMountHandler,
});
