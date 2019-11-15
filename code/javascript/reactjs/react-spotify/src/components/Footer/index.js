import React from 'react';
import { Layout } from 'antd';

const { Footer: FooterAntd } = Layout;

class Footer extends React.Component {
  render() {
    return (
      <FooterAntd style={{ textAlign: 'center' }}>
        <a href="http://opensource.org/licenses/BSD-3-Clause" target="_blank" rel="noopener noreferrer">
          New BSD License
        </a>
      </FooterAntd>
    );
  }
}

export default Footer;
