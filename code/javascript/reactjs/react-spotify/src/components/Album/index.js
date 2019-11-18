import React from 'react';
import { Card } from 'antd';
import PropTypes from 'prop-types';

const { Meta } = Card;

class Album extends React.Component {
  goTo = (url) => {
    window.open(url, '_blank');
  };

  render() {
    const artists = this.props.artists.join(', ');
    return (
      <Card
        key={this.props.id}
        hoverable
        style={{ width: 300 }}
        cover={<img alt={this.props.name} src={this.props.imageUrl} width="300" height="300" />}
        onClick={() => this.goTo(this.props.externalUrl)}
      >
        <Meta title={this.props.name} description={artists} />
      </Card>
    );
  }
}

Album.propTypes = {
  id: PropTypes.string,
  name: PropTypes.string,
  externalUrl: PropTypes.string,
  imageUrl: PropTypes.string,
  artists: PropTypes.array,
};

export default Album;
