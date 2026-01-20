#!/bin/bash
# This script initializes the Torn Companion App project.

echo "🚀 Starting Torn Companion App setup..."

# 1. Create directory structure
echo "📂 Creating project directories..."
mkdir -p src/lib/api src/components src/layouts src/pages

# 2. Create project configuration files
echo "📝 Creating configuration files..."

# package.json
cat <<'EOF' > package.json
{
  "name": "torn-companion",
  "type": "module",
  "version": "0.0.1",
  "scripts": {
    "dev": "astro dev",
    "start": "astro dev",
    "build": "astro build",
    "preview": "astro preview",
    "astro": "astro"
  },
  "dependencies": {
    "@astrojs/react": "^3.0.0",
    "@astrojs/tailwind": "^5.0.0",
    "@nanostores/persistent": "^0.9.1",
    "@nanostores/react": "^0.7.1",
    "@tanstack/react-query": "^4.35.3",
    "@tanstack/react-query-devtools": "^4.35.3",
    "@tanstack/react-table": "^8.9.11",
    "astro": "^3.1.0",
    "daisyui": "^3.7.7",
    "nanostores": "^0.9.3",
    "react": "^18.2.0",
    "react-dom": "^18.2.0",
    "tailwindcss": "^3.3.3"
  },
  "devDependencies": {
    "@types/react": "^18.2.22",
    "@types/react-dom": "^18.2.7",
    "typescript": "^5.2.2"
  }
}
EOF

# astro.config.mjs
cat <<'EOF' > astro.config.mjs
import { defineConfig } from 'astro/config';
import tailwind from "@astrojs/tailwind";
import react from "@astrojs/react";

// https://astro.build/config
export default defineConfig({
  integrations: [tailwind({
    config: {
      applyBaseStyles: false
    }
  }), react()],
  output: 'static'
});
EOF

# tailwind.config.cjs
cat <<'EOF' > tailwind.config.cjs
/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['./src/**/*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}'],
  theme: {
    extend: {},
  },
  plugins: [require("daisyui")],
  daisyui: {
    themes: ["light", "dark", "cupcake", "bumblebee", "emerald", "corporate", "synthwave", "retro", "cyberpunk", "valentine", "halloween", "garden", "forest", "aqua", "lofi", "pastel", "fantasy", "wireframe", "black", "luxury", "dracula", "cmyk", "autumn", "business", "acid", "lemonade", "night", "coffee", "winter"],
  },
}
EOF

# tsconfig.json
cat <<'EOF' > tsconfig.json
{
  "extends": "astro/tsconfigs/strict",
  "compilerOptions": {
    "jsx": "react-jsx",
    "jsxImportSource": "react",
    "baseUrl": ".",
    "paths": {
      "@/*": [
        "src/*"
      ]
    }
  }
}
EOF

# 3. Create library files (State and API clients)
echo "📚 Creating library files..."

# src/lib/store.ts
cat <<'EOF' > src/lib/store.ts
import { persistentMap } from '@nanostores/persistent';
import { atom } from 'nanostores';

// UI State
export const currentTab = atom('dashboard');
export const theme = persistentMap('ui:theme', { name: 'dark' });

// API Keys Storage
export const apiKeys = persistentMap('apiKeys:', {
  torn: '',
  yata: '',
  tornstats: '',
});

// Function to set the theme
export const setTheme = (newThemeName: string) => {
  theme.setKey('name', newThemeName);
  if (typeof window !== 'undefined') {
    document.documentElement.setAttribute('data-theme', newThemeName);
  }
};

// Initialize theme from localStorage on client-side
if (typeof window !== 'undefined') {
  const currentTheme = theme.get().name;
  document.documentElement.setAttribute('data-theme', currentTheme);
}
EOF

# src/lib/api/tornClient.ts
cat <<'EOF' > src/lib/api/tornClient.ts
export class TornClient {
  private apiKey: string;
  private static readonly API_URL = 'https://api.torn.com';

  constructor(apiKey: string) {
    if (!apiKey) {
      throw new Error('Torn API key is required for TornClient.');
    }
    this.apiKey = apiKey;
  }

  private async fetchData(path: string, params: Record<string, string> = {}) {
    const urlParams = new URLSearchParams({ ...params, key: this.apiKey });
    const url = `${TornClient.API_URL}/${path}?${urlParams.toString()}`;

    try {
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`Network response was not ok: ${response.statusText}`);
      }
      const data = await response.json();
      if (data.error) {
        throw new Error(`Torn API Error (Code ${data.error.code}): ${data.error.error}`);
      }
      return data;
    } catch (error) {
      console.error(`[TornClient] Failed to fetch from ${path}:`, error);
      throw error;
    }
  }

  // V1 "selections" based method (e.g., faction data)
  public async get<T = any>(endpoint: string, id: string, selections: string[]): Promise<T> {
    const path = `${endpoint}/${id}`;
    return this.fetchData(path, { selections: selections.join(',') });
  }

  // V2 "endpoint" based method (covers most new calls)
  public async call<T = any>(endpoint: string, params: Record<string, string> = {}): Promise<T> {
    return this.fetchData(endpoint, params);
  }
  
  // Specific helpers
  public async getFactionBasic() {
    return this.call('faction', { selections: 'basic' });
  }

  public async getFactionMembers() {
    const factionData = await this.getFactionBasic();
    const members = factionData.members;
    // Convert members object to array
    return Object.entries(members).map(([id, memberData]) => ({
      id,
      ...memberData as any,
    }));
  }
}
EOF

# src/lib/api/yataClient.ts
cat <<'EOF' > src/lib/api/yataClient.ts
export class YataClient {
  private apiKey: string;

  constructor(apiKey: string) {
    if (!apiKey) {
      console.warn('YataClient initialized without an API key.');
      this.apiKey = '';
    } else {
      this.apiKey = apiKey;
    }
  }

  public async getSomeData() {
    if (!this.apiKey) return Promise.resolve({ message: "YATA API key not provided." });
    // Example:
    // const response = await fetch(`https://yata.yt/api/v1/some/endpoint?key=${this.apiKey}`);
    // return response.json();
    console.log("Fetching data from YATA...");
    return Promise.resolve({ message: "YATA data placeholder" });
  }
}
EOF

# src/lib/api/tornStatsClient.ts
cat <<'EOF' > src/lib/api/tornStatsClient.ts
export class TornStatsClient {
  private apiKey: string;

  constructor(apiKey: string) {
    if (!apiKey) {
      console.warn('TornStatsClient initialized without an API key.');
      this.apiKey = '';
    } else {
      this.apiKey = apiKey;
    }
  }
  
  public async getSpyReports() {
    if (!this.apiKey) return Promise.resolve({ message: "TornStats API key not provided." });
    // Example:
    // const response = await fetch(`https://www.tornstats.com/api/v1/spy?key=${this.apiKey}`);
    // return response.json();
    console.log("Fetching data from TornStats...");
    return Promise.resolve({ message: "TornStats data placeholder" });
  }
}
EOF

# 4. Create React components
echo "⚛️ Creating React components..."

# src/components/StatusBar.tsx
cat <<'EOF' > src/components/StatusBar.tsx
import React from 'react';
import { useQueryClient } from '@tanstack/react-query';

const StatusBar = () => {
  const queryClient = useQueryClient();
  const [countdown, setCountdown] = React.useState(60);

  React.useEffect(() => {
    const timer = setInterval(() => {
      setCountdown(prev => (prev > 0 ? prev - 1 : 60));
    }, 1000);
    return () => clearInterval(timer);
  }, []);

  const handleRefresh = () => {
    queryClient.invalidateQueries();
    setCountdown(60);
  };

  return (
    <div className="flex items-center justify-end gap-4 p-2 bg-base-200 rounded-lg">
      <span className="text-sm">Next refresh in: {countdown}s</span>
      <button className="btn btn-xs btn-primary" onClick={handleRefresh}>
        Refresh Now
      </button>
    </div>
  );
};

export default StatusBar;
EOF

# src/components/FactionTable.tsx
cat <<'EOF' > src/components/FactionTable.tsx
import React, { useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useStore } from '@nanostores/react';
import {
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  getSortedRowModel,
  useReactTable,
  type SortingState,
} from '@tanstack/react-table';
import { TornClient } from '../lib/api/tornClient';
import { apiKeys } from '../lib/store';

type FactionMember = {
  id: string;
  name: string;
  level: number;
  status: {
    description: string;
    details: string;
    state: string;
    color: string;
    until: number;
  };
};

const columnHelper = createColumnHelper<FactionMember>();

const columns = [
  columnHelper.accessor('name', {
    header: 'Name',
    cell: info => info.getValue(),
  }),
  columnHelper.accessor('level', {
    header: 'Level',
    cell: info => info.getValue(),
  }),
  columnHelper.accessor(row => row.status.description, {
    id: 'status',
    header: 'Status',
    cell: info => info.getValue(),
  }),
];

const FactionTable = () => {
  const $apiKeys = useStore(apiKeys);
  const [sorting, setSorting] = React.useState<SortingState>([]);

  const tornClient = useMemo(() => {
    if (!$apiKeys.torn) return null;
    return new TornClient($apiKeys.torn);
  }, [$apiKeys.torn]);

  const { data, isLoading, error } = useQuery({
    queryKey: ['factionMembers', $apiKeys.torn],
    queryFn: async () => {
        if (!tornClient) throw new Error("Torn client not available.");
        return tornClient.getFactionMembers();
    },
    enabled: !!$apiKeys.torn && !!tornClient,
  });

  const table = useReactTable({
    data: data ?? [],
    columns,
    state: { sorting },
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
  });

  if (isLoading) return <div className="text-center p-4">Loading faction members... <span className="loading loading-spinner"></span></div>;
  if (error) return <div className="text-center p-4 text-error">Error: {error instanceof Error ? error.message : 'An unknown error occurred'}</div>;

  return (
    <div className="overflow-x-auto">
      <table className="table table-zebra w-full">
        <thead>
          {table.getHeaderGroups().map(headerGroup => (
            <tr key={headerGroup.id}>
              {headerGroup.headers.map(header => (
                <th key={header.id} onClick={header.column.getToggleSortingHandler()} className="cursor-pointer">
                  {flexRender(header.column.columnDef.header, header.getContext())}
                  {{ asc: ' 🔼', desc: ' 🔽' }[header.column.getIsSorted() as string] ?? ''}
                </th>
              ))}
            </tr>
          ))}
        </thead>
        <tbody>
          {table.getRowModel().rows.map(row => (
            <tr key={row.id}>
              {row.getVisibleCells().map(cell => (
                <td key={cell.id}>
                  {flexRender(cell.column.columnDef.cell, cell.getContext())}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default FactionTable;
EOF

# src/components/Settings.tsx
cat <<'EOF' > src/components/Settings.tsx
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
EOF

# 5. Create Astro layouts and pages
echo "🚀 Creating Astro layouts and pages..."

# src/layouts/MainLayout.astro
cat <<'EOF' > src/layouts/MainLayout.astro
---
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';
import Settings from '../components/Settings.tsx';
import { apiKeys } from '../lib/store';

const queryClient = new QueryClient();

// This check runs server-side. It reads from localStorage-backed store,
// so it will be empty on first build. The client-side logic handles the real check.
// We use a script to ensure the check happens client-side before rendering.
interface Props {
	title: string;
}

const { title } = Astro.props;
---

<!DOCTYPE html>
<html lang="en">
	<head>
		<meta charset="UTF-8" />
		<meta name="description" content="Torn Companion App" />
		<meta name="viewport" content="width=device-width" />
		<link rel="icon" type="image/svg+xml" href="/favicon.svg" />
		<meta name="generator" content={Astro.generator} />
		<title>{title}</title>
	</head>
	<body>
    <div id="app-wrapper" class="invisible">
      <QueryClientProvider client={queryClient} client:only="react">
        <slot />
        <ReactQueryDevtools initialIsOpen={false} client:only="react" />
      </QueryClientProvider>
    </div>
    
    <div id="settings-wrapper" class="invisible min-h-screen flex items-center justify-center bg-base-200">
        <Settings client:only="react" />
    </div>

    <script is:inline>
      // This script runs immediately on the client
      import { apiKeys } from '../lib/store';
      import { theme } from '../lib/store';

      // Set theme on initial load
      const currentTheme = theme.get().name;
      document.documentElement.setAttribute('data-theme', currentTheme);

      // Check for API key and toggle visibility
      const tornApiKey = apiKeys.get().torn;
      if (tornApiKey) {
        document.getElementById('app-wrapper').classList.remove('invisible');
      } else {
        document.getElementById('settings-wrapper').classList.remove('invisible');
      }
    </script>
	</body>
</html>
EOF

# src/pages/index.astro
cat <<'EOF' > src/pages/index.astro
---
import MainLayout from '../layouts/MainLayout.astro';
import FactionTable from '../components/FactionTable.tsx';
import StatusBar from '../components/StatusBar.tsx';
import { Icon } from 'astro-icon/components';
---

<MainLayout title="Torn Companion">
  <div class="navbar bg-base-100">
    <div class="flex-1">
      <a class="btn btn-ghost normal-case text-xl">Torn App</a>
    </div>
    <div class="flex-none">
      <a href="/settings" class="btn btn-square btn-ghost">
        <Icon name="mdi:cog" class="w-6 h-6" />
      </a>
    </div>
  </div>
	<main class="container mx-auto p-4">
    <StatusBar client:only="react" />

		<div class="mt-6 card bg-base-100 shadow-xl">
      <div class="card-body">
        <h2 class="card-title">Faction Members</h2>
        <FactionTable client:only="react" />
      </div>
		</div>
	</main>
</MainLayout>
EOF

# src/pages/settings.astro
cat <<'EOF' > src/pages/settings.astro
---
import MainLayout from '../layouts/MainLayout.astro';
import Settings from '../components/Settings.tsx';
---

<MainLayout title="Settings">
	<main class="container mx-auto p-4 flex justify-center items-center min-h-screen">
		<Settings client:only="react" />
	</main>
</MainLayout>
EOF

# Final instructions
echo ""
echo "✅ Setup complete!"
echo "Next steps:"
echo "1. Run 'bun install' to install dependencies."
echo "2. Run 'bun dev' to start the development server."
echo "3. Open your browser to http://localhost:4321."
echo ""
echo "Note: Astro Icon is used. You may need to install it ('npx astro add icon') and an icon pack (e.g., '@iconify-json/mdi')."

exit 0
