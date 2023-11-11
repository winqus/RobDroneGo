import { Injectable } from '@angular/core';
import { TEXT_TOKENS as content } from '../../assets/i18n/_textTokens';
import { DashboardProps } from '../features/dashboard/dashboard.component';

// Injectable service decorator
@Injectable({
  providedIn: 'root',
})
export class DashboardService {
  // Business logic of dashboard component:

  getDashboardData(): DashboardProps {
    return content.components.dashboard;
  }

  getCurrentDatetime(): string {
    return new Date().toLocaleString();
  }
}
