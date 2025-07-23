// src/App.tsx
import { Routes, Route } from 'react-router-dom'
import HomePage from './pages/Home'
import ListPage from './pages/List'
import ProofPage from './pages/Proof'
// import About from './pages/About'
// import NotFound from './pages/NotFound'

function App() {
  return (
    <>
      <Routes>
        <Route path="/" element={<HomePage />} />
        <Route path="/list" element={<ListPage />} />
        <Route path="/proof" element={<ProofPage />} />
        {/* catch-all 404: */}
        {/* <Route path="*" element={<NotFound />} /> */}
      </Routes>
    </>
  )
}

export default App