import { Component, Input, OnInit } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidatorFn, Validators } from '@angular/forms';
import { CreateRobotTypeData, RobotService } from 'src/app/services/robot.service';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface CreateRobotTypeProps {
  nameLabel: string;
  nameLabelPlaceholder: string;
  nameLabelRequiredError: string;

  brandLabel: string;
  brandLabelPlaceholder: string;
  brandLabelRequiredError: string;

  modelLabel: string;
  modelLabelPlaceholder: string;
  modelLabelRequiredError: string;

  typesOfTasksLabel: string;
  typesOfTasksPlaceholder: string;
  typesOfTasksRequiredError: string;

  submitButtonLabel: string;
  robotTypeCreatedMessage: string;
}

export function atLeastOneTaskValidator(tasksArray: string[]): ValidatorFn {
  return (): { [key: string]: boolean } | null => {
    if (tasksArray.length > 0) return null;
    return { atLeastOneTaskRequired: true };
  };
}

@Component({
  selector: 'app-create-robot-type',
  templateUrl: './create-robot-type.component.html',
  styleUrls: ['./create-robot-type.component.css'],
})
export class CreateRobotTypeComponent {
  @Input() props: CreateRobotTypeProps = this.getDefaultProps();

  tasks: string[] = [];
  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  robotTypeForm: FormGroup;

  constructor(private robotService: RobotService) {
    this.robotTypeForm = new FormGroup({
      name: new FormControl('', [Validators.required, Validators.maxLength(30)]),
      brand: new FormControl('', [Validators.required, Validators.maxLength(30)]),
      model: new FormControl('', [Validators.required, Validators.maxLength(30)]),
      typesOfTasks: new FormControl('', [atLeastOneTaskValidator(this.tasks)]),
    });
  }

  getDefaultProps(): CreateRobotTypeProps {
    return {
      nameLabel: 'Robot Type Name',
      nameLabelPlaceholder: 'Enter Robot Type Name',
      nameLabelRequiredError: 'Robot Type Name is required',

      brandLabel: 'Brand',
      brandLabelPlaceholder: 'Enter Brand',
      brandLabelRequiredError: 'Brand is required',

      modelLabel: 'Model',
      modelLabelPlaceholder: 'Enter Model',
      modelLabelRequiredError: 'Model is required',

      typesOfTasksLabel: 'Types of Tasks',
      typesOfTasksPlaceholder: 'Enter Types of Tasks',
      typesOfTasksRequiredError: 'At least one type of task is required',

      submitButtonLabel: 'Create Robot Type',
      robotTypeCreatedMessage: 'Robot Type created successfully',
    };
  }

  addTask() {
    const taskFormControl = this.robotTypeForm.get('typesOfTasks');
    if (taskFormControl && taskFormControl.value && typeof taskFormControl.value === 'string') {
      this.tasks.push(taskFormControl.value.trim());
      taskFormControl.setValue('');
    }
  }

  removeTask(index: number) {
    this.tasks.splice(index, 1);
    const taskFormControl = this.robotTypeForm.get('typesOfTasks');
    if (taskFormControl) {
      taskFormControl.setErrors(atLeastOneTaskValidator(this.tasks)(taskFormControl));
    }
  }

  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;
    const robotTypeData: CreateRobotTypeData = this.robotTypeForm.value;
    robotTypeData.typesOfTasks = this.tasks;
    console.log(robotTypeData);

    this.robotService.createRobotType(robotTypeData).subscribe({
      next: () => {
        this.submitSuccessMessage = this.props.robotTypeCreatedMessage;
        this.isLoading = false;
        this.robotTypeForm.reset();
        this.tasks = [];
      },
      error: (error) => {
        console.error('Error creating robot type:', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
