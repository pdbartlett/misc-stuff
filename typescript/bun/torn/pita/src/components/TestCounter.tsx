import { useState } from 'react';

export default function TestCounter() {
  const [count, setCount] = useState(0);

  return (
    <div className="p-4 border border-base-300 rounded-lg bg-base-200">
      <h3 className="font-bold text-lg mb-2">React Check</h3>
      <p className="mb-4">Count: <span className="text-primary font-bold">{count}</span></p>
      <button 
        className="btn btn-primary btn-sm"
        onClick={() => setCount(count + 1)}
      >
        Increment
      </button>
    </div>
  );
}