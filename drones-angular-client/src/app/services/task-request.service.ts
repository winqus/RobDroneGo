import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import { DeliveryTask } from '../core/models/deliveryTask.model';
import { TaskFilters } from '../core/models/shared/taskFilters.type';
import { SurveillanceTask } from '../core/models/surveillanceTask.model';
import { TaskRequest, TaskStatus } from '../core/models/taskRequest.model';

export interface CreateTaskRequestDTO {
  requesterEmail: string;
  task: SurveillanceTask | DeliveryTask;
}

export interface UpdateTaskRequestStatusDTO {
  status: string;
}

@Injectable({
  providedIn: 'root',
})
export class TaskRequestService {
  constructor(private http: HttpClient) {}

  getAllTaskRequests(): Observable<TaskRequest[]> {
    const route = API_ROUTES.taskRequest.getAll;

    return this.http.get<TaskRequest[]>(route);
  }

  getTaskRequestById(id: string): Observable<TaskRequest[]> {
    const route = API_ROUTES.taskRequest.getById(id);

    return this.http.get<TaskRequest[]>(route);
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
      status: Object.entries(TaskStatus).find(([key, value]) => value === status)?.[0] as string
    };
    console.log(statusDTO);
    const route = API_ROUTES.taskRequest.updateStatus(id);

    return this.http.patch<TaskRequest>(route, statusDTO);
  }

  // getTaskByFilter(filters: TaskFilters): TaskRequest[] {
  //   const definedFilters = Object.fromEntries(Object.entries(filters).filter(([_, v]) => v !== undefined));
  //   const route = API_ROUTES.taskRequest.getAll;

  //   return filteredTasks;
  // }
}
