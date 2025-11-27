// @ts-check
import { defineConfig } from 'astro/config';

import preact from '@astrojs/preact';

// https://astro.build/config
export default defineConfig({
  site: "https://pdbartlett.github.io/pita",
  base: "/pita",
  trailingSlash: "never",
  integrations: [preact()],
  build: {
    assets: "genfiles"
  }
});