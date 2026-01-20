import React, { useState } from 'react';
import { useStore } from '@nanostores/react';
import { apiKeys } from '../lib/store';

const Settings = () => {
  const $apiKeys = useStore(apiKeys);
  const [localKeys, setLocalKeys] = useState($apiKeys);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value } = e.target;
    setLocalKeys(prev => ({ ...prev, [name]: value }));
  };

  const handleSave = () => {
    apiKeys.set(localKeys);
    alert('API Keys saved successfully! The page will now reload to apply the changes.');
    window.location.reload();
  };

  return (
    <div className="p-4 max-w-md mx-auto card bg-base-100 shadow-xl">
      <div className="card-body">
        <h2 className="card-title">API Key Settings</h2>
        <p className="text-sm text-base-content/70">
          A Torn API key is required. Get one from{' '}
          <a href="https://www.torn.com/preferences.php#tab=api" target="_blank" rel="noopener noreferrer" className="link link-primary">
            Torn's preferences page
          </a>.
        </p>

        <div className="form-control w-full mt-4">
          <label className="label"><span className="label-text font-bold">Torn API Key</span></label>
          <input
            type="text"
            name="torn"
            value={localKeys.torn}
            onChange={handleChange}
            className="input input-bordered w-full"
            placeholder="Required"
          />
        </div>
        <div className="form-control w-full mt-2">
          <label className="label"><span className="label-text">YATA API Key</span></label>
          <input
            type="text"
            name="yata"
            value={localKeys.yata}
            onChange={handleChange}
            className="input input-bordered w-full"
            placeholder="Optional"
          />
        </div>
        <div className="form-control w-full mt-2">
          <label className="label"><span className="label-text">TornStats API Key</span></label>
          <input
            type="text"
            name="tornstats"
            value={localKeys.tornstats}
            onChange={handleChange}
            className="input input-bordered w-full"
            placeholder="Optional"
          />
        </div>
        <div className="card-actions justify-end mt-6">
            <button onClick={handleSave} className="btn btn-primary">
                Save & Reload
            </button>
        </div>
      </div>
    </div>
  );
};

export default Settings;
