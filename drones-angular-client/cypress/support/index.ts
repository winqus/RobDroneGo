export {}

declare global {
  namespace Cypress {
    interface Chainable {
      /**
       * Logs in E2E user
       * @returns void
       */
      login: (username: string, password: string) => void;
    }
  }
}