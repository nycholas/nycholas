import React from 'react';
import PropTypes from 'prop-types';
import { useSelector } from 'react-redux';
import { Layout, Breadcrumb } from 'antd';

import { globalSelectors } from '../../services/global';

const { Content: ContentAntd } = Layout;

function Content(props) {
  const breadcrums = useSelector(globalSelectors.getBreadcrumbs);

  const breadcrumbRender = () => {
    const items = breadcrums.map((it) => <Breadcrumb.Item key={it}>{it}</Breadcrumb.Item>);
    return <Breadcrumb style={{ margin: '16px 0' }}>{items}</Breadcrumb>;
  };

  return (
    <ContentAntd style={{ padding: '0 50px' }}>
      {breadcrumbRender()}
      <div style={{ background: '#fff', padding: 24, minHeight: 280 }}>{props.children}</div>
    </ContentAntd>
  );
}

Content.propTypes = {
  children: PropTypes.object,
};

export default Content;
