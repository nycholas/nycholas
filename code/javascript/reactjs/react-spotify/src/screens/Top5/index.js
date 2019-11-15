import React from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import { Skeleton, Empty, Divider } from 'antd';

import Artist from '../../components/Artist';
import { globalCreators } from '../../services/global';
import { meCreators, meSelectors } from '../../services/me';

class Top5 extends React.Component {
  componentDidMount() {
    this.props.screenDidMount(['Home', 'Top5']);
    this.props.fetchTop5Artists();
  }

  loadingRender() {
    const list = [...Array(5).keys()];
    const loading = list.map((n) => <Skeleton key={n} />);

    return <>{loading}</>;
  }

  emptyRender() {
    return <Empty />;
  }

  successRender() {
    const artists = this.props.artists;
    if (!artists) {
      return this.emptyRender();
    }

    const cards = artists.map((artist) => (
      <div key={artist.id}>
        <Artist
          id={artist.id}
          name={artist.name}
          imageUrl={artist.image.url}
          externalUrl={artist.externalUrl}
          genres={artist.genres}
          followers={artist.followers}
          popularity={artist.popularity}
        />
        <Divider />
      </div>
    ));

    return <>{cards}</>;
  }

  errorRender() {
    return (
      <Empty
        description="Oops, something went wrong!"
        image="https://gw.alipayobjects.com/mdn/miniapp_social/afts/img/A*pevERLJC9v0AAAAAAAAAAABjAQAAAQ/original"
        imageStyle={{
          height: 60,
        }}
      />
    );
  }

  render() {
    const cards = this.props.loading ? this.loadingRender() : this.successRender();
    if (this.props.failed) {
      return this.errorRender();
    }

    return <>{cards}</>;
  }
}

Top5.propTypes = {
  failed: PropTypes.bool,
  loading: PropTypes.bool,
  artists: PropTypes.array,
  screenDidMount: PropTypes.func,
  fetchTop5Artists: PropTypes.func,
};

const mapStateToProps = (state) => ({
  failed: meSelectors.isFailed(state),
  loading: meSelectors.isLoading(state),
  artists: meSelectors.getArtists(state),
});

const mapDispatchToProps = (dispatch) => ({
  screenDidMount: (breadcrumbs) => dispatch(globalCreators.globalScreenDidMount(breadcrumbs)),
  fetchTop5Artists: () => dispatch(meCreators.meTop5FetchRequested()),
});

export default connect(mapStateToProps, mapDispatchToProps)(Top5);
