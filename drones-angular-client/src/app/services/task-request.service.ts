import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from '../../api.config';
import { DeliveryTask } from '../core/models/deliveryTask.model';
import { TaskFilters } from '../core/models/shared/taskFilters.type';
import { SurveillanceTask } from '../core/models/surveillanceTask.model';
import { PlanningError, TaskPlan, TaskPlanningStatus } from '../core/models/taskPlan.model';
import { TaskRequest, TaskStatus } from '../core/models/taskRequest.model';

export interface CreateTaskRequestDTO {
  requesterEmail: string;
  task: SurveillanceTask | DeliveryTask;
}

export interface UpdateTaskRequestStatusDTO {
  status: string;
}

export interface PostPlanTasksRequest {
  taskRequestIds: string[];
}

export interface PostPlanTasksResponse {
  message: string;
  state: 'unstarted' | 'planning' | 'planned' | 'error';
}

export type PlanningResponse = TaskPlan | PlanningError;

@Injectable({
  providedIn: 'root',
})
export class TaskRequestService {
  constructor(private http: HttpClient) {}

  getAllTaskRequests(): Observable<TaskRequest[]> {
    const route = API_ROUTES.taskRequest.getAll;

    return this.http.get<TaskRequest[]>(route);
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
      status: Object.entries(TaskStatus).find(([key, value]) => value === status)?.[0] as string,
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

  requestTaskPlanning(taskRequestIDs: string[]): Observable<PostPlanTasksResponse> {
    const route = API_ROUTES.taskRequest.requestPlanning;
    const requestData: PostPlanTasksRequest = {
      taskRequestIds: taskRequestIDs,
    };

    return this.http.post<PostPlanTasksResponse>(route, requestData);
  }

  getTaskPlanningStatus(): Observable<TaskPlanningStatus> {
    const route = API_ROUTES.taskRequest.planningStatus;

    return this.http.get<TaskPlanningStatus>(route);
  }

  getTaskPlanningResults(): Observable<PlanningResponse> {
    const route = API_ROUTES.taskRequest.planningResults;

    return this.http.get<PlanningResponse>(route);
  }
}
