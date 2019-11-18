import React from 'react';
import { Provider } from 'react-redux';
import { BrowserRouter as Router } from 'react-router-dom';

import store from './store';
import Template from './screens';
import { axiosConfig } from './services/config';

function App() {
  axiosConfig();

  return (
    <Provider store={store}>
      <Router>
        <Template />
      </Router>
    </Provider>
  );
}

export default App;
