import { Observable } from '@nativescript/core'

export class HomeViewModel extends Observable {
  constructor() {
    super()
  }
  htmlString = `
    <h1 style="color: black; font-family: ui-sans-serif, system-ui;">
      <span style="color: #65adf1;">Html</span>View
    </h1>
    `
}
