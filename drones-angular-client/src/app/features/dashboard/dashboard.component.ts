import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { DashboardService } from 'src/app/services/dashboard.service';

export interface DashboardProps {
  leftSideBar?: {
    text: string;
  };
  mainContent: {
    text: string;
  };
  rightSideBar?: {
    text: string;
  };
}

@Component({
  selector: 'app-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.css'],
})
export class DashboardComponent implements OnInit, OnDestroy {
  // To make props passable from parent to child component, use @Input() decorator
  @Input() props!: DashboardProps;

  // Properties
  currentDate!: string;
  private timerId!: number;

  // Inject DashboardService
  constructor(private dashboardService: DashboardService) { }

  // On initialization lifecycle hook
  ngOnInit(): void {
    this.props = this.getDefaultProps();
    this.updateDateTime();
    this.timerId = window.setInterval(() => {
      this.updateDateTime();
    }, 1000);
  }

  // At the end of the component lifecycle, clean up
  ngOnDestroy(): void {
    if (this.timerId) {
      clearInterval(this.timerId);
    }
  }

  // Get dashboard props
  getDefaultProps(): DashboardProps {
    return (
      this.dashboardService.getDashboardData() || {
        mainContent: {
          text: 'No props',
        },
      }
    );
  }

  // Other methods

  private updateDateTime(): void {
    this.currentDate = this.dashboardService.getCurrentDatetime();
  }

  getMainContentClasses(): string {
    if (this.props.leftSideBar && this.props.rightSideBar) {
      return 'col-span-8';
    } else if (this.props.leftSideBar || this.props.rightSideBar) {
      return 'col-span-10';
    } else {
      return 'col-span-12';
    }
  }
}
