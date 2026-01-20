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
