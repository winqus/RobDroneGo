import { CreateTaskRequestDTO } from 'src/app/services/task-request.service';
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

      createRoom: (floorId: string, roomName: string, size: [number, number], position: [number, number], category?: string) => void;

      createTaskRequest: (taskRequestDTO: CreateTaskRequestDTO) => void;

      createSurveillanceTaskRequest: (requesterEmail: string, buildingCode: string, floorNumber: number[], contactNumber?: number) => void;
    }
  }
}
