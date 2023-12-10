import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import { DeliveryTask } from '../core/models/deliveryTask.model';
import { SurveillanceTask } from '../core/models/surveillanceTask.model';
import { TaskRequest, TaskStatus } from '../core/models/taskRequest.model';

export interface CreateTaskRequestDTO {
  requesterEmail: string;
  task: SurveillanceTask | DeliveryTask;
}

export interface UpdateTaskRequestStatusDTO {
  state: string;
}

@Injectable({
  providedIn: 'root',
})
export class TaskRequestService {
  constructor(private http: HttpClient) {}

  getAllTaskRequests(): Observable<TaskRequest> {
    const route = API_ROUTES.taskRequest.getAll;

    return this.http.get<TaskRequest>(route);
  }

  getTaskRequestById(id: string): Observable<TaskRequest> {
    const route = API_ROUTES.taskRequest.getById(id);

    return this.http.get<TaskRequest>(route);
  }

  createTaskRequest(taskRequest: CreateTaskRequestDTO): Observable<TaskRequest> {
    const taskRequestDTO: CreateTaskRequestDTO = {
      requesterEmail: taskRequest.requesterEmail,
      task: taskRequest.task,
    };
    const route = API_ROUTES.taskRequest.create;

    return this.http.post<TaskRequest>(route, taskRequestDTO);
  }

  updateTaskRequestStatus(id: string, status: TaskStatus): Observable<TaskRequest> {
    const statusDTO: UpdateTaskRequestStatusDTO = {
      state: status.toString(),
    };
    const route = API_ROUTES.taskRequest.updateStatus(id);

    return this.http.put<TaskRequest>(route, statusDTO);
  }
}
