import { environment } from "./environments/environment";

const API_BASE = environment.apiUrl;

/* Usage example:
    getBuildingByCode(id: number) {
      return this.http.get<Building>(API_ROUTES.building.getByCode(id));
    }
*/

export const API_ROUTES = {
  base: API_BASE,
  user: {
    me: `${API_BASE}/auth/me`,
    login: `${API_BASE}/auth/signin`,
    register: `${API_BASE}/auth/signup`,
    update: `${API_BASE}/auth/user`,
    logout: `${API_BASE}/auth/logout`,
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
};

