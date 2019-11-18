module.exports = {
  env: {
    browser: true,
    node: true,
    es6: true,
    jest: true
  },
  extends: [
    'react-app',
    'wiremore',
    'wiremore/react',
    'prettier',
    'prettier/react',
    'plugin:import/errors',
    'plugin:import/warnings',
    'plugin:prettier/recommended',
    'plugin:security/recommended',
  ],
  globals: {
    __BROWSER__: true,
  },
  parser: 'babel-eslint',
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: 'module'
  },
  plugins: ['babel', 'import', 'prettier', 'security'],
  settings: {
    'import/resolver': {
      node: {
        paths: ['src/'],
      },
    },
    react: {
      version: 'detect',
    },
  },
  rules: {
    'import/named': 0,
    'import/no-unassigned-import': 0,
    'import/no-named-as-default-member': 0,
    'prettier/prettier': 'error',
    'no-empty': [2, { allowEmptyCatch: true }]
  },
  'overrides': [
    {
      'files': ['src/components/Album/index.js'],
      'rules': {
        'security/detect-non-literal-fs-filename': 'off'
      }
    }
  ]
};
