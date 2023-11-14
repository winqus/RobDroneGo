import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import Robot from '../core/models/robot.model';
import { RobotFilters } from '../core/models/shared/robotFilters.type';
import RobotType from '../core/models/robotType.model';

@Injectable({
  providedIn: 'root',
})
export class RobotService {
  constructor(private http: HttpClient) {}

  createRobot(robot: Robot): Observable<Robot> {
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

  createRobotType(robotType: RobotType): Observable<RobotType> {
    const route = API_ROUTES.robot.createRobotType;
    const postRobotType = {
      name: robotType.name,
      brand: robotType.brand,
      model: robotType.model,
      tyoesOfTasks: robotType.typesOfTasks,
    };
    return this.http.post<RobotType>(route, postRobotType);
  }
}