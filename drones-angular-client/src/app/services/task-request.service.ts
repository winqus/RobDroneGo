import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';
import { TaskRequest } from '../core/models/taskRequest.model';

@Injectable({
  providedIn: 'root',
})
export class TaskRequestService {
  constructor(private http: HttpClient) {}

  getAllTaskRequests(): Observable<TaskRequest> {
    return this.http.get<TaskRequest>(API_ROUTES.taskRequest.getAll);
  }

  getTaskRequestById(id: string): Observable<TaskRequest> {
    return this.http.get<TaskRequest>(API_ROUTES.taskRequest.getById(id));
  }

  createTaskRequest(taskRequest: TaskRequest): Observable<TaskRequest> {
    return this.http.post<TaskRequest>(API_ROUTES.taskRequest.create, taskRequest);
  }

  updateTaskRequestStatus(id: string, status: string): Observable<TaskRequest> {
    return this.http.put<TaskRequest>(API_ROUTES.taskRequest.updateStatus(id), { status });
  }
}
