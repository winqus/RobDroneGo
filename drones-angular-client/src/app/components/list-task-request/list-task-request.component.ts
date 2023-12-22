import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { TaskRequest, TaskStatus } from '../../core/models/taskRequest.model';
import { TaskRequestService } from '../../services/task-request.service';

@Component({
  selector: 'app-list-task-request',
  templateUrl: './list-task-request.component.html',
  styleUrls: ['./list-task-request.component.css'],
})
export class ListTaskRequestComponent implements OnInit {
  pending = Object.entries(TaskStatus).find(([key, value]) => value === TaskStatus.Pending)?.[0];
  taskRequests: TaskRequest[] = [];
  taskRequestsFullData: TaskRequest[] = [];

  isDelivery(taskRequest: TaskRequest): boolean {
    return 'pickUpRoomId' in taskRequest.task && 'deliveryRoomId' in taskRequest.task;
  }

  hasNavigationData(taskRequest: TaskRequest): boolean {
    return taskRequest.navigationData !== null && taskRequest.navigationData !== undefined;
  }

  constructor(
    private taskRequestService: TaskRequestService,
    private router: Router,
  ) {}

  ngOnInit(): void {
    this.taskRequestService.getAllTaskRequests().subscribe((taskRequests) => {
      this.taskRequestsFullData = taskRequests;
      this.taskRequests = taskRequests;
    });
  }

  previewNavigationData(taskRequest: TaskRequest): void {
    // this.router.navigate([`3d?taskId=${taskRequest.id}`]);
    // this.router.navigate([`3d?taskId=${5}`]);
    this.router.navigate(['/3d'], { queryParams: { taskId: taskRequest.id } });
  }

  isPending(taskRequest: TaskRequest): boolean {
    return taskRequest.status.toString() === this.pending;
  }

  approveRequest(taskRequest: TaskRequest): void {
    this.taskRequestService.updateTaskRequestStatus(taskRequest.id, TaskStatus.Approved).subscribe((taskRequest) => {
      const task = this.taskRequests.find((taskRequestItem) => taskRequestItem.id === taskRequest.id);
      if (task) {
        task.status = taskRequest.status;
      }
    });
  }

  denyRequest(taskRequest: TaskRequest): void {
    this.taskRequestService.updateTaskRequestStatus(taskRequest.id, TaskStatus.Denied).subscribe((taskRequest) => {
      const task = this.taskRequests.find((taskRequestItem) => taskRequestItem.id === taskRequest.id);
      if (task) {
        task.status = taskRequest.status;
      }
    });
  }

  onPendingToggle(event: Event): void {
    const isChecked = (event.target as HTMLInputElement).checked;
    this.taskRequests = this.taskRequestsFullData.filter((taskRequest: TaskRequest) => (isChecked ? taskRequest.status.toString() === this.pending : true));
    console.log(this.taskRequests);
  }
}
