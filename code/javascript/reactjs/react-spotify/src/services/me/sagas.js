import { call, put, takeLatest } from 'redux-saga/effects';

import * as api from './api';
import { Creators, Types } from './store';

function* meTop5Saga() {
  try {
    const result = yield call(api.top5);
    const artists = result.items.map((item) => ({
      id: item.id,
      name: item.name,
      image: item.images.find((img) => img.width === 160 && img.height === 160),
      externalUrl: item.external_urls.spotify,
      genres: item.genres,
      followers: item.followers.total,
      popularity: item.popularity,
    }));
    yield put(Creators.meTop5FetchSuccess(artists));
  } catch (e) {
    yield put(Creators.meTop5FetchFailed(e.message, e.reason));
  }
}

export default [takeLatest(Types.ME_TOP5_FETCH_REQUESTED, meTop5Saga)];
