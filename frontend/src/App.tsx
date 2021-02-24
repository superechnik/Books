import './App.css';
import './components/navBar';
import NavBar from './components/navBar';
import BookGrid from './components/Book/bookGrid';
import { isConstructorTypeNode } from 'typescript';

function App() {
  return (
    <div className="App">
        <NavBar />
          <BookGrid />
     </div>
  );
}

export default App;
