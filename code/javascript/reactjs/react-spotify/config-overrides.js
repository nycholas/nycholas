const rewireStyledComponents = require('react-app-rewire-styled-components');
const { override, fixBabelImports, addLessLoader } = require('customize-cra');

const styledComponents = (obj) => (config) => {
  config = rewireStyledComponents(config, process.env.NODE_ENV, obj);
  return config;
};

module.exports = override(
  fixBabelImports('import', {
    libraryName: 'antd',
    libraryDirectory: 'es',
    style: true,
  }),
  addLessLoader({
    importLoaders: true,
    javascriptEnabled: true,
    modifyVars: {},
  }),
  styledComponents({
    displayName: process.env.NODE_ENV !== 'production',
  })
);
