import { RobotFilters } from './app/core/models/shared/robotFilters.type';
import { environment } from './environments/environment';

const API_BASE = environment.apiUrl;

/* Usage example:
    getBuildingByCode(id: number) {
      return this.http.get<Building>(API_ROUTES.building.getByCode(id));
    }
*/

export const API_ROUTES = {
  base: API_BASE,
  // threeDModuleAssetsBase: './assets',
  threeDModuleAssetsBase: 'https://robdrongoassets.blob.core.windows.net/angular-assets',
  user: {
    me: `${API_BASE}/auth/me`,
    login: `${API_BASE}/auth/signin`,
    register: `${API_BASE}/auth/signup`,
    update: `${API_BASE}/auth/update`,
    logout: `${API_BASE}/auth/logout`,
    getAll: `${API_BASE}/auth/all`,
    confirm: `${API_BASE}/auth/confirm`,
    delete: `${API_BASE}/auth/delete`,
  },
  building: {
    getAll: `${API_BASE}/building/all`,
    getByCode: (buildingCode: string) => `${API_BASE}/building/${buildingCode}`,
    getByFloorRange: (minFloor: number, maxFloor: number) => `${API_BASE}/building?minFloor=${minFloor}&maxFloor=${maxFloor}`,
    create: `${API_BASE}/building`,
    update: (id: string) => `${API_BASE}/building/${id}`,
  },
  elevator: {
    createElevator: (buildingCode: string) => `${API_BASE}/building/${buildingCode}/elevator`,
    updateElevator: (buildingCode: string) => `${API_BASE}/building/${buildingCode}/elevator`,
    listElevators: (buildingCode: string) => `${API_BASE}/building/${buildingCode}/elevators`,
  },
  passage: {
    createPassage: `${API_BASE}/passage`,
    getPassages: `${API_BASE}/passage`,
    getPassagesBetweenBuildings: (buildingCode1: string, buildingCode2: string) => `${API_BASE}/passage?buildingCode1=${buildingCode1}&buildingCode2=${buildingCode2}`,
    listFloorsWithPassagesToDifferentBuilding: (buildingCode: string) => `${API_BASE}/passage/toDifferentBuildings?buildingCode=${buildingCode}`,
    updatePassage: `${API_BASE}/passage`,
  },
  robot: {
    getAll: `${API_BASE}/robot`,
    getByFilter: (filters: any) => `${API_BASE}/robot?${new URLSearchParams(filters as any).toString()}`,
    //Example: const url = API_ROUTES.robot.getByFilter({ type: "service", brand: "AcmeRobotics", model:"X200"});
    update: (robotCode: string) => `${API_BASE}/robot/${robotCode}/state`,
    createRobot: `${API_BASE}/robot`,
    createRobotType: `${API_BASE}/robotType`,
    changeRobotState: (robotCode: string) => `${API_BASE}/robot/${robotCode}/state`,
  },
  floor: {
    createFloor: `${API_BASE}/floor`,
    getByBuildingCode: (buildingCode: string) => `${API_BASE}/floor/${buildingCode}`,
    getAllFloors: `${API_BASE}/floor`,
    updateFloor: (id: string) => `${API_BASE}/floor/${id}`,
    floorWithElevator: (code: string) => `${API_BASE}/floor/elevator?buildingCode=${code}`,
  },
  room: {
    createRoom: `${API_BASE}/room`,
    getAllRooms: `${API_BASE}/room/all`,
  },
  map: {
    uploadMap: (buildingCode: string, floorNumber: number) => `${API_BASE}/floor/${floorNumber}/building/${buildingCode}/`,
    getMap: (buildingCode: string, floorNumber: number) => `${API_BASE}/floor/${floorNumber}/building/${buildingCode}/map`,
  },
  folder: {
    upload: `${API_BASE}/folder/upload?file`,
    list: `${API_BASE}/folder/`,
  },
  planning: {
    upload: `${API_BASE}/folder/upload?file`,
    list: `${API_BASE}/folder/`,
    roomsNavigation: `${API_BASE}/planning/calculate-cells`,
  },
};
