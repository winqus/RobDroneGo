import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import Building from '../core/models/building.model';

@Injectable({
  providedIn: 'root'
})
export class BuildingService {

  constructor(private http: HttpClient) { }

  getAllBuildings(): Observable<Building[]> {
    const route = API_ROUTES.building.getAll;
    return this.http.get<Building[]>(route);
  }

  getBuildingByCode(buildingCode: string): Observable<Building> {
    const route = API_ROUTES.building.getByCode(buildingCode);
    return this.http.get<Building>(route);
  }

  getBuildingsByFloorRange(minFloor: number, maxFloor: number): Observable<Building[]> {
    const route = API_ROUTES.building.getByFloorRange(minFloor, maxFloor);
    return this.http.get<Building[]>(route);
  }

  createBuilding(building: Building): Observable<Building> {
    const route = API_ROUTES.building.create;
    const postBuilding = {
      name: building.name,
      code: building.code,
      description: building.description,
      floorSizeLength: building.floorSizeLength,
      floorSizeWidth: building.floorSizeWidth,
    };
    return this.http.post<Building>(route, postBuilding);
  }

  updateBuilding(id: string, building: Partial<Building>): Observable<Building> {
    const route = API_ROUTES.building.update(id);
    return this.http.put<Building>(route, building);
  }
}
