import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import Passage from '../core/models/passage.model'; 

@Injectable({
  providedIn: 'root'
})
export class PassageService {
  constructor(private http: HttpClient) { }

  createPassage(passage: Passage): Observable<Passage> {
    const route = API_ROUTES.passage.createPassage;
    return this.http.post<Passage>(route, passage);
  }

  getPassages(): Observable<Passage[]> {
    const route = API_ROUTES.passage.getPassages;
    return this.http.get<Passage[]>(route);
  }

  getPassagesBetweenBuildings(buildingCode1: string, buildingCode2: string): Observable<Passage[]> {
    const route = API_ROUTES.passage.getPassagesBetweenBuildings(buildingCode1, buildingCode2);
    return this.http.get<Passage[]>(route);
  }

  listFloorsWithPassagesToDifferentBuilding(buildingCode: string): Observable<any> {
    const route = API_ROUTES.passage.listFloorsWithPassagesToDifferentBuilding(buildingCode);
    return this.http.get<any>(route);
  }

  updatePassage(oldPassage: Partial<Passage>, newPassage: Partial<Passage>): Observable<Passage> {
    const route = API_ROUTES.passage.updatePassage;
    return this.http.put<Passage>(route, { oldPassage, newPassage });
  }
  
}
