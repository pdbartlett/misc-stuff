import { NativeScriptConfig } from '@nativescript/core';

export default {
  id: 'uk.co.justdabbling.dashy',
  appPath: 'app',
  appResourcesPath: 'App_Resources',
  android: {
    v8Flags: '--expose_gc',
    markingMode: 'none'
  }
} as NativeScriptConfig;