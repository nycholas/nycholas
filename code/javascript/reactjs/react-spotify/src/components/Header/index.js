import React from 'react';
import PropTypes from 'prop-types';
import { Layout, Menu } from 'antd';
import { withRouter } from 'react-router-dom';

import { Logo } from './styled-wrappers';

const { Header: HeaderAntd } = Layout;

class Header extends React.Component {
  goTo = (path) => {
    this.props.history.push(path);
  };

  render() {
    return (
      <HeaderAntd>
        <Logo>
          <a
            href="https://github.com/nycholas/nycholas/tree/master/code/javascript/reactjs/react-spotify"
            target="_blank"
            rel="noopener noreferrer"
          >
            @nycholas
          </a>
        </Logo>
        <Menu
          theme="dark"
          mode="horizontal"
          defaultSelectedKeys={[this.props.history.location.pathname]}
          style={{ lineHeight: '64px' }}
        >
          <Menu.Item key="/" onClick={() => this.goTo('/')}>
            New Releases
          </Menu.Item>
          <Menu.Item key="/recommendations" onClick={() => this.goTo('/recommendations')}>
            Recommendations
          </Menu.Item>
          <Menu.Item key="/top5" onClick={() => this.goTo('/top5')}>
            Top Artists and Tracks
          </Menu.Item>
        </Menu>
      </HeaderAntd>
    );
  }
}

Header.propTypes = {
  history: PropTypes.object,
};

export default withRouter(Header);
