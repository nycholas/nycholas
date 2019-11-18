import React from 'react';
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import { Row, Col, Card, Empty } from 'antd';

import Album from '../../components/Album';
import { globalCreators } from '../../services/global';
import { browseCreators, browseSelectors } from '../../services/browse';

class NewRelease extends React.Component {
  componentDidMount() {
    this.props.screenDidMount(['Home', 'New Releases']);
    this.props.fetchNewReleaseAlbums();
  }

  loadingRender() {
    const list = [...Array(20).keys()];
    const loadingCards = list.map((n) => (
      <Col key={n}>
        <Card style={{ width: 300 }} loading={true} />
      </Col>
    ));

    return <>{loadingCards}</>;
  }

  emptyRender() {
    return <Empty />;
  }

  successRender() {
    const albums = this.props.albums;
    if (!albums) {
      return this.emptyRender();
    }

    const cards = albums.map((album) => (
      <Col key={album.id}>
        <Album
          id={album.id}
          name={album.name}
          externalUrl={album.externalUrl}
          artists={album.artists}
          imageUrl={album.image.url}
        />
      </Col>
    ));

    return <>{cards}</>;
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
    const cards = this.props.loading ? this.loadingRender() : this.successRender();
    if (this.props.failed) {
      return this.failedRender();
    }

    return (
      <div>
        <Row gutter={[16, 32]} type="flex" justify="space-around" align="middle">
          {cards}
        </Row>
      </div>
    );
  }
}

NewRelease.propTypes = {
  failed: PropTypes.bool,
  loading: PropTypes.bool,
  albums: PropTypes.array,
  screenDidMount: PropTypes.func,
  fetchNewReleaseAlbums: PropTypes.func,
};

const mapStateToProps = (state) => ({
  failed: browseSelectors.isFailed(state),
  loading: browseSelectors.isLoading(state),
  albums: browseSelectors.getAlbums(state),
});

const mapDispatchToProps = (dispatch) => ({
  screenDidMount: (breadcrumbs) => dispatch(globalCreators.globalScreenDidMount(breadcrumbs)),
  fetchNewReleaseAlbums: () => dispatch(browseCreators.browseNewReleasesFetchRequested()),
});

export default connect(mapStateToProps, mapDispatchToProps)(NewRelease);
