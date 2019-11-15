import React from 'react';
import PropTypes from 'prop-types';
import { Row, Col, Descriptions } from 'antd';

class Artist extends React.Component {
  render() {
    const genres = this.props.genres.join(', ');
    return (
      <div>
        <Row key={this.props.id}>
          <Col span={18} push={6}>
            <Descriptions layout="vertical" column={2}>
              <Descriptions.Item label="Artist">
                <a href={this.props.externalUrl} target="_blank" rel="noopener noreferrer">
                  {this.props.name}
                </a>
              </Descriptions.Item>
              <Descriptions.Item label="Genres">{genres}</Descriptions.Item>
              <Descriptions.Item label="Followers">{this.props.followers}</Descriptions.Item>
              <Descriptions.Item label="popularity">{this.props.popularity}</Descriptions.Item>
            </Descriptions>
          </Col>
          <Col span={6} pull={18}>
            <img src={this.props.imageUrl} height="160" width="160" alt={this.props.name} />
          </Col>
        </Row>
      </div>
    );
  }
}

Artist.propTypes = {
  id: PropTypes.string,
  name: PropTypes.string,
  imageUrl: PropTypes.string,
  externalUrl: PropTypes.string,
  genres: PropTypes.array,
  followers: PropTypes.number,
  popularity: PropTypes.number,
};

export default Artist;
