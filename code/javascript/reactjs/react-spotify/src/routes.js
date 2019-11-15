import React from 'react';
import { Route, Switch } from 'react-router-dom';

import Top5 from './screens/Top5';
import NewRelease from './screens/NewRelease';
import Recommendation from './screens/Recommendation';

function Router() {
  return (
    <Switch>
      <Route exact path="/" component={NewRelease} />
      <Route exact path="/recommendations" component={Recommendation} />
      <Route exact path="/top5" component={Top5} />
    </Switch>
  );
}

export default Router;
