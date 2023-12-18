import { Component, EventEmitter, Input, Output } from '@angular/core';
import { FormControl, FormGroup } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { TaskFilters } from 'src/app/core/models/shared/taskFilters.type';
import { TaskRequest } from 'src/app/core/models/taskRequest.model';
import { TaskRequestService } from 'src/app/services/task-request.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface SearchTaskProps {
  statusLabel: string;
  statusPlaceholder: string;

  deviceLabel: string;
  devicePlaceholder: string;

  userLabel: string;
  userPlaceholder: string;

  listTasksButtonLabel: string;

  searchResults: TaskRequest[];
  searchResultsPlaceholder: string;
}

@Component({
  selector: 'app-search-task',
  templateUrl: './search-task.component.html',
  styleUrls: ['./search-task.component.css'],
})
export class SearchTaskComponent {
  [x: string]: any;
  @Input() props: SearchTaskProps = this.getDefaultProps();
  @Output() submitEvent = new EventEmitter<unknown>();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  noTasksFoundMessage: string = 'No tasks found';
  isLoading = false;
  loadedOnce = false;
  searchTaskForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(
    private taskRequestService: TaskRequestService,
    private router: Router,
    private route: ActivatedRoute,
  ) {
    this.searchTaskForm = new FormGroup({
      status: new FormControl(''),
      device: new FormControl(''),

      user: new FormControl(''),
      searchResults: new FormControl(''),
    });
  }

  getDefaultProps(): SearchTaskProps {
    return {
      statusLabel: 'Status',
      statusPlaceholder: 'Enter task status',

      deviceLabel: 'Device',
      devicePlaceholder: 'Enter device',

      userLabel: 'User',
      userPlaceholder: 'Enter user email',

      searchResults: [],
      searchResultsPlaceholder: 'Search results',

      listTasksButtonLabel: 'List Tasks',
    };
  }

  getTasksByFilter(queryParams: TaskFilters) {
    this.router.navigate([], {
      relativeTo: this.route,
      queryParams,
      queryParamsHandling: 'merge',
    });

    this.taskRequestService.getAllTaskRequests().subscribe({
      next: (data) => {
        this.props.searchResults = data;

        if (queryParams.status) {
          this.props.searchResults = this.props.searchResults.filter((task) => task.status == queryParams.status);
        }

        if (queryParams.userEmail) {
          this.props.searchResults = this.props.searchResults.filter((task) => task.requesterEmail == queryParams.userEmail);
        }

        if (queryParams.robotTypeName) {
          //TODO
        }

        this.loadedOnce = true;
        this.isLoading = false;
      },
      error: (error) => {
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }

  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;

    const { status, device, user } = this.searchTaskForm.value;

    const queryParams: TaskFilters = {
      status: status || undefined,
      robotTypeName: device || undefined,
      userEmail: user || undefined,
    };

    this.getTasksByFilter(queryParams);
  }
}
