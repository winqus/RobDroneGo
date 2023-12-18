import { TaskStatus } from '../taskRequest.model';

export type TaskFilters = {
  status?: TaskStatus;
  robotTypeName?: string;
  userEmail?: string;
};
