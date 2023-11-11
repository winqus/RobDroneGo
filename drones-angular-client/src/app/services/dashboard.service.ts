import { Injectable } from '@angular/core';
import { TEXT_TOKENS as content } from '../../assets/i18n/_textTokens';
import DashboardProps from '../core/models/features/dashboard.interface';

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
