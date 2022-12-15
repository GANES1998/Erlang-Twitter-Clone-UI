import logo from './logo.svg';
import './App.css';
import useDocumentTitle from "./hooks/useDocumentTitle";
import {BrowserRouter} from "react-router-dom";
import NavBar from "./nav/NavBar";
import MainLayout from "./layout/MainLayout";
import {StaticDataContextProvider} from "./context/StaticDataContext";

function App() {
  useDocumentTitle("Twitter Clone")

  return (
    <div className="App">
        <StaticDataContextProvider>
          <BrowserRouter>
            <NavBar />
            <MainLayout />
          </BrowserRouter>
        </StaticDataContextProvider>
    </div>
  );
}

export default App;
