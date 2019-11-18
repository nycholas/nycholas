import { call, put, takeLatest } from 'redux-saga/effects';

import * as api from './api';
import { Creators, Types } from './store';

function* recommendationsSaga() {
  try {
    const result = yield call(api.recommendations);
    const tracks = result.tracks.map((track) => ({
      id: track.id,
      name: track.name,
      externalUrl: track.external_urls.spotify,
      explicit: track.explicit,
      album: {
        name: track.album.name,
        releaseDate: track.album.release_date,
        artists: track.album.artists.map((art) => art.name),
      },
    }));
    yield put(Creators.recommendationsFetchSuccess(tracks));
  } catch (e) {
    yield put(Creators.recommendationsFetchFailed(e.message, e.reason));
  }
}

export default [takeLatest(Types.RECOMMENDATIONS_FETCH_REQUESTED, recommendationsSaga)];
