import { call, put, takeLatest } from 'redux-saga/effects';

import * as api from './api';
import { Creators, Types } from './store';

function* newReleasesSaga() {
  try {
    const result = yield call(api.newReleases);
    const albums = result.albums.items.map((item) => ({
      id: item.id,
      name: item.name,
      image: item.images.find((img) => img.width === 300 && img.height === 300),
      artists: item.artists.map((art) => art.name),
      externalUrl: item.external_urls.spotify,
    }));
    yield put(Creators.browseNewReleasesFetchSuccess(albums));
  } catch (e) {
    yield put(Creators.browseNewReleasesFetchFailed(e.message, e.reason));
  }
}

export default [takeLatest(Types.BROWSE_NEW_RELEASES_FETCH_REQUESTED, newReleasesSaga)];
