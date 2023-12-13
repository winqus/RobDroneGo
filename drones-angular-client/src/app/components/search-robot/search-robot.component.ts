import { Component, EventEmitter, Input, Output } from '@angular/core';
import { FormBuilder, FormControl, FormGroup } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import Robot from 'src/app/core/models/robot.model';
import { RobotFilters } from 'src/app/core/models/shared/robotFilters.type';
import { RobotService } from 'src/app/services/robot.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface SearchRobotProps {
  codeLabel: string;
  codePlaceholder: string;

  typeNameLabel: string;
  typeNamePlaceholder: string;

  brandLabel: string;
  brandPlaceholder: string;

  modelLabel: string;
  modelPlaceholder: string;

  typesOfTasksLabel: string;
  typesOfTasksPlaceholder: string;

  listRobotsButtonLabel: string;

  searchResults: Robot[];
  searchResultsPlaceholder: string;
}

@Component({
  selector: 'app-search-robot',
  templateUrl: './search-robot.component.html',
  styleUrls: ['./search-robot.component.css'],
})
export class SearchRobotComponent {
  @Input() props: SearchRobotProps = this.getDefaultProps();
  @Output() submitEvent = new EventEmitter<unknown>();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  noRobotsFoundMessage: string = 'No robots found';
  isLoading = false;
  loadedOnce = false;
  searchRobotForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(
    private robotService: RobotService,
    private router: Router,
    private route: ActivatedRoute,
  ) {
    this.searchRobotForm = new FormGroup({
      typeName: new FormControl(''),
      brand: new FormControl(''),
      model: new FormControl(''),
      typesOfTasks: new FormControl(''),
      searchResults: new FormControl(''),
    });
  }

  getDefaultProps(): SearchRobotProps {
    return {
      codeLabel: 'Code',
      codePlaceholder: 'Enter robot code',

      typeNameLabel: 'Name of Robot Type ',
      typeNamePlaceholder: 'Enter robot type name',

      brandLabel: 'Brand',
      brandPlaceholder: 'Enter brand',

      modelLabel: 'Model',
      modelPlaceholder: 'Enter model',

      typesOfTasksLabel: 'Type of task',
      typesOfTasksPlaceholder: 'Enter one type of task',

      searchResults: [],
      searchResultsPlaceholder: 'Search results',

      listRobotsButtonLabel: 'List Robots',
    };
  }

  getRobotByFilter(queryParams: RobotFilters) {
    this.router.navigate([], {
      relativeTo: this.route,
      queryParams,
      queryParamsHandling: 'merge',
    });

    this.robotService.getRobotByFilter(queryParams).subscribe({
      next: (data) => {
        this.props.searchResults = data;
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

    const { name: typeName, type, brand, model, typesOfTasks } = this.searchRobotForm.value;

    const queryParams: RobotFilters = {
      typeName: typeName || undefined,
      brand: brand || undefined,
      model: model || undefined,
      typesOfTasks: typesOfTasks?.split(',') || undefined,
    };

    if (queryParams.typesOfTasks?.length === 1 && queryParams.typesOfTasks[0] === '') {
      queryParams.typesOfTasks = undefined;
    }

    this.getRobotByFilter(queryParams);
  }
}
