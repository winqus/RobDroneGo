import { Component, EventEmitter, Input, Output } from '@angular/core';
import { FormBuilder, FormControl, FormGroup } from '@angular/forms';
import Robot from 'src/app/core/models/robot.model';
import { RobotService } from 'src/app/services/robot.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface SearchRobotProps {
  codeLabel: string;
  codePlaceholder: string;

  nicknameLabel: string;
  nicknamePlaceholder: string;

  serialNumberLabel: string;
  serialNumberPlaceholder: string;

  descriptionLabel: string;
  descriptionPlaceholder: string;

  typeLabel: string;
  typePlaceholder: string;

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
  noBuildingsFoundMessage: string = '';
  isLoading = false;
  loadedOnce = false;
  searchRobotForm: FormGroup;
  validationErrors = content.validation_errors;
  router: any;
  activatedRoute: any;

  constructor(private robotService: RobotService) {
    this.searchRobotForm = new FormGroup({
      code: new FormControl(''),
      nickname: new FormControl(''),
      serialNumber: new FormControl(''),
      description: new FormControl(''),
      type: new FormControl(''),
      typesOfTasks: new FormControl(''),
      searchResults: new FormControl(''),
    });
  }

  getDefaultProps(): SearchRobotProps {
    return {
      codeLabel: 'Code',
      codePlaceholder: 'Enter robot code',

      nicknameLabel: 'Nickname',
      nicknamePlaceholder: 'Enter nickname',

      serialNumberLabel: 'Serial number',
      serialNumberPlaceholder: 'Enter serial number',

      descriptionLabel: ' Description',
      descriptionPlaceholder: 'Enter description',

      typeLabel: 'Type',
      typePlaceholder: 'Enter type',

      typesOfTasksLabel: 'Type of tasks',
      typesOfTasksPlaceholder: 'Enter types of tasks',

      searchResults: [],
      searchResultsPlaceholder: 'Search results',

      listRobotsButtonLabel: 'List Robots',
    };
  }

  getRobotByFilter(code?: string, nickname?: string, serialNumber?: string, description?: string, type?: string, typesOfTasks?: string) {
    const queryParams = {
      code,
      nickname,
      serialNumber,
      description,
      type,
      typesOfTasks,
    };

    this.router.navigate([], {
      relativeTo: this.activatedRoute,
      queryParams,
      queryParamsHandling: 'merge',
    });
  }

  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;

    const { code, nickname, serialNumber, description, type, typesOfTasks } = this.searchRobotForm.value;

    this.getRobotByFilter(code, nickname, serialNumber, description, type, typesOfTasks);
  }
}
