import { NavigationPlan } from 'src/app/core/models/shared/navigationPlan.interface';

export interface RobotState {
  isAutoMoving: boolean;
  navigationData?: NavigationPlan;
  navigationState?: 'unstarted' | 'ready' | 'started' | 'stepFinished' | 'fullyCompleted' | 'stopped' | 'error';
  navigationStep?: number;
  numberOfNavigationSteps?: number;
}
