import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import Room from '../core/models/room.model';

@Injectable({
  providedIn: 'root',
})
export class RoomService {
  constructor(private http: HttpClient) {}

  createRoom(room: Room): Observable<Room> {
    const route = API_ROUTES.room.createRoom;
    const postRoom = {
      name: room.name,
      description: room.description,
      size: room.size,
      position: room.position,
      category: room.category,
      floorId: room.floorId,
    };
    return this.http.post<Room>(route, postRoom);
  }

  getAllRooms(): Observable<Room[]> {
    const route = API_ROUTES.room.getAllRooms;
    return this.http.get<Room[]>(route);
  }
}
