import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import { environment } from 'src/environments/environment';
import { NavigationPlan } from '../core/models/shared/navigationPlan.interface';

export interface RoomsPathQueryParams {
  origin_building_code: string;
  origin_floor_number: number;
  origin_room: string;
  destination_building_code: string;
  destination_floor_number: number;
  destination_room: string;
  minimize_elevator_uses: boolean;
  minimize_building_count: boolean;
}

@Injectable({
  providedIn: 'root',
})
export class PlanningService {
  constructor(private http: HttpClient) {}

  calculateCells(data: RoomsPathQueryParams): Observable<NavigationPlan> {
    const url = API_ROUTES.planning.roomsNavigation;
    const queryParams = {
      origin_building_code: data.origin_building_code,
      origin_floor_number: data.origin_floor_number,
      origin_room: data.origin_room,
      destination_building_code: data.destination_building_code,
      destination_floor_number: data.destination_floor_number,
      destination_room: data.destination_room,
      minimize_elevator_uses: data.minimize_elevator_uses,
      minimize_building_count: data.minimize_building_count,
    };

    return this.http.get<NavigationPlan>(url, { params: queryParams });
  }
}
