import React from 'react';
import { Layout } from 'antd';

import Routes from '../routes';
import Header from '../components/Header';
import Footer from '../components/Footer';
import Content from '../components/Content';

function Template() {
  return (
    <Layout className="layout">
      <Header />
      <Layout>
        <Content>
          <Routes />
        </Content>
      </Layout>
      <Footer />
    </Layout>
  );
}

export default Template;
