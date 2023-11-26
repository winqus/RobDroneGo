import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, map } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import Building from '../core/models/building.model';
import Elevator from '../core/models/elevator.model';

export interface CreateElevatorData {
  number: number;
  floors: number[];
  make?: string;
  model?: string;
  serialNumber?: string;
  description?: string;
}

interface UpdateElevatorData {
  make: string;
  model: string;
  serialNumber: string;
  description: string;
}

@Injectable({
  providedIn: 'root',
})
export class ElevatorService {
  constructor(private http: HttpClient) {}

  createElevator(buildingCode: string, elevatorData: CreateElevatorData): Observable<Elevator> {
    const route = API_ROUTES.elevator.createElevator(buildingCode);
    return this.http.post<Elevator>(route, elevatorData);
  }

  updateElevator(buildingCode: string, elevatorData: UpdateElevatorData): Observable<Elevator> {
    const route = API_ROUTES.elevator.updateElevator(buildingCode);
    return this.http.put<Elevator>(route, elevatorData);
  }

  listElevators(buildingCode: string): Observable<Elevator[]> {
    const route = API_ROUTES.elevator.listElevators(buildingCode);
    return this.http.get<Elevator[]>(route);
  }

  getElevator(buildingCode: string): Observable<Elevator | null> {
    const route = API_ROUTES.building.getByCode(buildingCode);
    return this.http.get<Building>(route).pipe(
      map((building: Building) => {
        return building.elevator ? (building.elevator as Elevator) : null;
      }),
    );
  }
}
