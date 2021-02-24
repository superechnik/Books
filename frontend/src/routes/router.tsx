import {
    BrowserRouter as Router,
    Route,
    Switch,
  } from 'react-router-dom';
  import App from '../App'

const Routes = (props:any) =>
    <Router>
        <div>
            <Switch>
                <Route path="/" component={App} />
                <Route
                path="/stub"
                render={() => <h1>This is a stub page</h1>} />
                <Route render={() => <h1>There's nothing here!</h1>} />
            </Switch>
        </div>
    </Router>

export default Routes;