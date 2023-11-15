import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import Floor from '../core/models/floor.model';

export interface CreateFloorData {
  floorNumber: number;
  description?: string;
  buildingCode: string;
}

@Injectable({
  providedIn: 'root',
})
export class FloorService {
  constructor(private http: HttpClient) {}

  getFloorsByBuildingCode(buildingCode: string): Observable<Floor[]> {
    const route = API_ROUTES.floor.getByBuildingCode(buildingCode);
    return this.http.get<Floor[]>(route);
  }

  createFloor(floor: CreateFloorData): Observable<Floor> {
    const route = API_ROUTES.floor.createFloor;
    const postFloor = {
      floorNumber: floor.floorNumber,
      description: floor.description,
      buildingCode: floor.buildingCode,
    };
    return this.http.post<Floor>(route, postFloor);
  }

  updateFloor(id: string, floor: Partial<Floor>): Observable<Floor> {
    const route = API_ROUTES.floor.updateFloor(id);
    return this.http.put<Floor>(route, floor);
  }

  getAllFloors(): Observable<Floor[]> {
    const route = API_ROUTES.floor.getAllFloors;
    return this.http.get<Floor[]>(route);
  }
}
