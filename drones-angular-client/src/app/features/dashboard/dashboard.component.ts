import { HttpClient, HttpRequest, HttpResponse } from '@angular/common/http';
import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import * as marked from 'marked';
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
  convertedHtml: SafeHtml = '';

  // To make props passable from parent to child component, use @Input() decorator
  @Input() props!: DashboardProps;

  // Properties
  currentDate!: string;
  private timerId!: number;

  // Inject DashboardService
  constructor(
    private dashboardService: DashboardService,
    private http: HttpClient,
    private sanitizer: DomSanitizer,
  ) {}

  // On initialization lifecycle hook
  ngOnInit(): void {
    this.props = this.getDefaultProps();
    this.updateDateTime();
    this.timerId = window.setInterval(() => {
      this.updateDateTime();
    }, 1000);

    const url = './assets/info/info.md'; // Updated path to info.md
    const req = new HttpRequest('GET', url, {
      responseType: 'text',
    });

    this.http.request(req).subscribe({
      next: (event) => {
        if (event instanceof HttpResponse) {
          const markdownContent = event.body;

          this.convertedHtml = this.sanitizer.bypassSecurityTrustHtml(this.markdownToHtml(markdownContent));
        }
      },
      error: (error) => {
        console.error('Error fetching info text:', error);
      },
    });
  }

  markdownToHtml(markdown: any) {
    return marked.parse(markdown);
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
