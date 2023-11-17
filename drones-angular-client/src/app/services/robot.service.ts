import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import Robot from '../core/models/robot.model';
import RobotType from '../core/models/robotType.model';
import { RobotFilters } from '../core/models/shared/robotFilters.type';

export interface CreateRobotData {
  code: string;
  description?: string;
  nickname: string;
  serialNumber: string;
  type: string;
}

export interface CreateRobotTypeData {
  name: string;
  brand: string;
  model: string;
  typesOfTasks: string[];
}

@Injectable({
  providedIn: 'root',
})
export class RobotService {
  constructor(private http: HttpClient) {}

  createRobot(robot: CreateRobotData): Observable<Robot> {
    const route = API_ROUTES.robot.createRobot;
    const postRobot = {
      code: robot.code,
      nickname: robot.nickname,
      serialNumber: robot.serialNumber,
      description: robot.description,
      type: robot.type,
    };
    return this.http.post<Robot>(route, postRobot);
  }

  getRobot(): Observable<Robot[]> {
    const route = API_ROUTES.robot.getAll;
    return this.http.get<Robot[]>(route);
  }

  getRobotByFilter(filters: RobotFilters): Observable<Robot[]> {
    const route = API_ROUTES.robot.getByFilter(filters);
    return this.http.get<Robot[]>(route);
  }

  updateRobot(id: string, robot: Partial<Robot>): Observable<Robot> {
    const route = API_ROUTES.robot.update(id);
    return this.http.put<Robot>(route, robot);
  }

  createRobotType(robotType: CreateRobotTypeData): Observable<RobotType> {
    const route = API_ROUTES.robot.createRobotType;
    return this.http.post<RobotType>(route, robotType);
  }
}
