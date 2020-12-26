import {
    BrowserRouter as Router,
    Route,
    Switch,
  } from 'react-router-dom';
  import React from 'react';
  import App from '../App'

const Routes = () =>
    <Router>
        <div>
            <Switch>
                <Route path="/" component={App} />
                <Route
                path="/contact"
                render={() => <h1>Contact Us</h1>} />
                <Route render={() => <h1>Page not found</h1>} />
            </Switch>
        </div>
    </Router>

export default Routes;