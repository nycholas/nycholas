import { createSelector } from 'reselect';

const getState = (globalState) => globalState.globalReducers;

export const getBreadcrumbs = createSelector([getState], (state) => state.breadcrumbs);
