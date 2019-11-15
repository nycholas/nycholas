import React from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import { Table, Tag, Empty } from 'antd';

import { globalCreators } from '../../services/global';
import { recommendationCreators, recommendationSelectors } from '../../services/recommendation';

class Recommendation extends React.Component {
  componentDidMount() {
    this.props.screenDidMount(['Home', 'Recommendations']);
    this.props.fetchRecommendationTracks();
  }

  loadingRender() {
    return <Table loading={true} />;
  }

  emptyRender() {
    return <Empty />;
  }

  successRender() {
    const tracks = this.props.tracks;
    if (!tracks) {
      return this.emptyRender();
    }

    const columns = [
      {
        title: 'Name',
        dataIndex: 'name',
        key: 'name',
        render: (obj) => (
          <a href={obj.externalUrl} target="_black">
            {obj.name}
          </a>
        ),
      },
      {
        title: 'Album',
        dataIndex: 'album',
        key: 'album',
      },
      {
        title: 'Artists',
        dataIndex: 'artists',
        key: 'artists',
        render: (artists) => <span>{artists}</span>,
      },
      {
        title: 'Tags',
        key: 'tags',
        dataIndex: 'tags',
        render: (tags) => (
          <span>
            {tags.map((tag) => (
              <Tag color={'red'} key={tag}>
                {tag.toUpperCase()}
              </Tag>
            ))}
          </span>
        ),
      },
      {
        title: 'Release Date',
        dataIndex: 'releaseDate',
        key: 'releaseDate',
      },
    ];

    const data = tracks.map((track) => ({
      key: track.id,
      name: { name: track.name, externalUrl: track.externalUrl },
      album: track.album.name,
      artists: track.album.artists,
      tags: track.explicit ? ['explicit'] : [],
      releaseDate: track.album.releaseDate,
    }));

    return <Table columns={columns} dataSource={data} />;
  }

  failedRender() {
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
    const table = this.props.loading ? this.loadingRender() : this.successRender();
    if (this.props.failed) {
      return this.failedRender();
    }

    return <>{table}</>;
  }
}

Recommendation.propTypes = {
  failed: PropTypes.bool,
  loading: PropTypes.bool,
  tracks: PropTypes.array,
  screenDidMount: PropTypes.func,
  fetchRecommendationTracks: PropTypes.func,
};

const mapStateToProps = (state) => ({
  failed: recommendationSelectors.isFailed(state),
  loading: recommendationSelectors.isLoading(state),
  tracks: recommendationSelectors.getTracks(state),
});

const mapDispatchToProps = (dispatch) => ({
  screenDidMount: (breadcrumbs) => dispatch(globalCreators.globalScreenDidMount(breadcrumbs)),
  fetchRecommendationTracks: () => dispatch(recommendationCreators.recommendationsFetchRequested()),
});

export default connect(mapStateToProps, mapDispatchToProps)(Recommendation);
