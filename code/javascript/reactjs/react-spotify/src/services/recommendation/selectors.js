import { createSelector } from 'reselect';

const getState = (globalState) => globalState.recommendationReducers;

export const isLoading = createSelector([getState], (state) => state.loading);

export const isFailed = createSelector([getState], (state) => state.failed);

export const getError = createSelector([getState], (state) => state.error);

export const getTracks = createSelector([getState], (state) => state.tracks);
