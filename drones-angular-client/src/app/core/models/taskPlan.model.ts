export interface TaskPlan {
  data: TaskPlanData[];
}

export interface TaskPlanData {
  costOfChange: number;
  robotId: string;
  status: string;
  tasks: string;
  type: 'taskPlan';
}

export interface TaskPlanningStatus {
  message: string;
  state: 'unstarted' | 'planning' | 'planned' | 'error';
}

export interface PlanningError {
  error: string;
}
