import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import Map from '../core/models/map.model';

@Injectable({
  providedIn: 'root',
})
export class MapService {
  constructor(private http: HttpClient) {}

  uploadMap(buildingCode: string, floorNumber: number, map: Map): Observable<any> {
    const route = API_ROUTES.map.uploadMap(buildingCode, floorNumber);
    return this.http.patch(route, map);
  }

  getMap(buildingCode: string, floorNumber: number): Observable<Map> {
    const route = API_ROUTES.map.getMap(buildingCode, floorNumber);
    return this.http.get<Map>(route);
  }
}
