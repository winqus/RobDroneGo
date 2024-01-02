import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Subject, filter, interval, pairwise, switchMap, takeUntil } from 'rxjs';
import { DeliveryTask } from '../../core/models/deliveryTask.model';
import Robot from '../../core/models/robot.model';
import { SurveillanceTask } from '../../core/models/surveillanceTask.model';
import { TaskPlan, TaskPlanData, TaskPlanningStatus } from '../../core/models/taskPlan.model';
import { TaskRequest, TaskStatus } from '../../core/models/taskRequest.model';
import { RobotService } from '../../services/robot.service';
import { TaskRequestService } from '../../services/task-request.service';

interface PlannedTaskGroup extends TaskPlanData {
  taskRequests: TaskRequest[];
  robot: Robot;
}

@Component({
  selector: 'app-list-task-request',
  templateUrl: './list-task-request.component.html',
  styleUrls: ['./list-task-request.component.css'],
})
export class ListTaskRequestComponent implements OnInit {
  pending = Object.entries(TaskStatus).find(([key, value]) => value === TaskStatus.Pending)?.[0];
  denied = Object.entries(TaskStatus).find(([key, value]) => value === TaskStatus.Denied)?.[0];
  approved = Object.entries(TaskStatus).find(([key, value]) => value === TaskStatus.Approved)?.[0];

  taskRequests: TaskRequest[] = [];
  taskRequestsFullData: TaskRequest[] = [];
  plannedTaskRequestIDs: string[] = [];
  nonPlannedTaskRequests: TaskRequest[] = [];

  planningResults: TaskPlanData[] = [];

  plannedTaskGroups: PlannedTaskGroup[] = [];
  pendingTasks: TaskRequest[] = [];
  deniedTasks: TaskRequest[] = [];

  private isPlanningInProgressSubject = new BehaviorSubject<boolean>(true);
  isPlanningInProgress$ = this.isPlanningInProgressSubject.asObservable();

  showOnlyPending = false;

  private destroy$ = new Subject<void>();

  isDelivery(taskRequest: TaskRequest): boolean {
    return 'pickUpRoomId' in taskRequest.task && 'deliveryRoomId' in taskRequest.task;
  }

  isDeliveryTask(task: DeliveryTask | SurveillanceTask): task is DeliveryTask {
    return (task as DeliveryTask).description !== undefined;
  }

  isSurveillanceTask(task: DeliveryTask | SurveillanceTask): task is SurveillanceTask {
    return (task as SurveillanceTask).floorNumber !== undefined;
  }

  hasNavigationData(taskRequest: TaskRequest): boolean {
    return taskRequest.navigationData !== null && taskRequest.navigationData !== undefined;
  }

  constructor(
    private taskRequestService: TaskRequestService,
    private router: Router,
    private robotService: RobotService,
  ) {
    this.startPolling();
  }

  ngOnInit(): void {
    this.taskRequestService.getTaskPlanningStatus().subscribe({
      next: (taskPlanningStatus) => {
        if (taskPlanningStatus.state === 'planned') {
          this.getPlannedTasks();
        } else {
          this.initTasks();
        }
      },
      error: (error) => {
        console.error(error);
        this.initTasks();
      },
    });
  }

  ngOnDestroy() {
    this.destroy$.next();
    this.destroy$.complete();
  }

  initTasks(): void {
    // console.warn('plannedTasks', this.planningResults);
    this.robotService.getRobot().subscribe((robots) => {
      this.taskRequestService.getAllTaskRequests().subscribe({
        next: (taskRequests: TaskRequest[]) => {
          const sortedTaskRequests = taskRequests.sort((a, b) => {
            const dateA = new Date(a.requestCreatedDateTime).getTime();
            const dateB = new Date(b.requestCreatedDateTime).getTime();

            return dateB - dateA;
          });

          if (JSON.stringify(this.taskRequestsFullData) === JSON.stringify(sortedTaskRequests)) {
            return;
          }

          this.taskRequests = sortedTaskRequests;
          this.taskRequestsFullData = this.taskRequests;
          this.plannedTaskRequestIDs = [];
          if (this.planningResults.length > 0) {
            this.plannedTaskGroups = this.planningResults.map((planningResult) => {
              const groupRobot = robots.find((robot) => robot.id === planningResult.robotId);
              const taskRequestIDs = planningResult.tasks.replace('[', '').replace(']', '').split(',');
              const taskRequestsOfGroup: TaskRequest[] = taskRequestIDs.map((taskRequestID: string) => this.taskRequests.find((taskRequest: TaskRequest) => taskRequest.id === taskRequestID)!);
              this.plannedTaskRequestIDs = [...this.plannedTaskRequestIDs, ...taskRequestIDs];
              return {
                ...planningResult,
                taskRequests: taskRequestsOfGroup,
                robot: groupRobot!,
              };
            });
          }

          this.taskRequests = this.taskRequests.filter((taskRequest: TaskRequest) => !this.plannedTaskRequestIDs.includes(taskRequest.id));
          this.nonPlannedTaskRequests = this.taskRequests;
          // console.warn('plannedTaskGroups', this.plannedTaskGroups);
        },
        error: (error) => {
          console.error(error);
        },
      });
    });
  }

  startPolling() {
    interval(3000)
      .pipe(
        switchMap(() => this.taskRequestService.getTaskPlanningStatus()),
        takeUntil(this.destroy$),
      )
      .subscribe({
        next: (planningStatus: TaskPlanningStatus) => {
          this.isPlanningInProgressSubject.next(planningStatus.state === 'planning');
        },
        error: (error) => {
          console.error(error);
        },
      });

    this.isPlanningInProgress$
      .pipe(
        pairwise(),
        filter(([prevValue, currentValue]) => prevValue === true && currentValue === false),
        takeUntil(this.destroy$),
      )
      .subscribe(() => {
        this.getPlannedTasks();
      });
  }

  getPlannedTasks(): void {
    this.taskRequestService.getTaskPlanningResults().subscribe((planningResults) => {
      if ('data' in planningResults) {
        this.planningResults = planningResults.data;
        this.initTasks();
      } else {
        this.planningResults = [];
        console.error(planningResults.error);
        this.initTasks();
      }
    });
  }

  previewNavigationData(taskRequest: TaskRequest): void {
    this.router.navigate(['/3d'], { queryParams: { taskId: taskRequest.id } });
  }

  isPending(taskRequest: TaskRequest): boolean {
    return taskRequest.status.toString() === this.pending;
  }

  isDenied(taskRequest: TaskRequest): boolean {
    return taskRequest.status.toString() === this.denied;
  }

  isApproved(taskRequest: TaskRequest): boolean {
    return taskRequest.status.toString() === this.approved;
  }

  toggleExpandedAllTasks(expanded: boolean): void {
    const checkboxes = document.querySelectorAll('#taskItemCheckbox');
    checkboxes.forEach((checkbox) => {
      (checkbox as HTMLInputElement).checked = expanded;
    });
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
    this.showOnlyPending = isChecked;
    this.nonPlannedTaskRequests = this.taskRequests.filter((taskRequest: TaskRequest) => (isChecked ? taskRequest.status.toString() === this.pending : true));
  }

  planTasks(): void {
    this.isPlanningInProgressSubject.next(true);

    const validStatuses = ['Approved', 'Planned'];
    const tasksToPlanIDs = this.taskRequestsFullData.filter((taskRequest: TaskRequest) => validStatuses.includes(taskRequest.status.toString())).map((taskRequest: TaskRequest) => taskRequest.id);
    console.log('tasksToPlanIDs', tasksToPlanIDs);
    this.taskRequestService.requestTaskPlanning(tasksToPlanIDs).subscribe({
      next: (response) => {
        console.log('Planning response', response);
      },
      error: (error) => {
        console.error(error);
      },
    });
  }
}
