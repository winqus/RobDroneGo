import LoginCredentials from './loginCreds.interface';

export {};

declare global {
  namespace Cypress {
    interface Chainable {
      /**
       * Logs in E2E user
       * @returns void
       */
      login: (loginCreds: LoginCredentials) => void;

      createBuilding: (buildingCode: string, floorSizeLength?: number, floorSizeWidth?: number) => void;

      createFloor: (buildingCode: string, floorNumber: number) => void;
    }
  }
}
