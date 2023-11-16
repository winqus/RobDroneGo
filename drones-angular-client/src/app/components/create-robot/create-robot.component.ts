import { Component, Input, OnInit } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import Robot from 'src/app/core/models/robot.model';
import RobotType from 'src/app/core/models/robotType.model';
import { CreateRobotData, RobotService } from 'src/app/services/robot.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface CreateRobotProps {
  robotCodeLabel: string;
  robotCodePlaceholder: string;
  robotCodeRequiredError: string;

  robotNicknameLabel: string;
  robotNicknamePlaceholder: string;
  robotNicknameRequiredError: string;

  robotSerialNumberLabel: string;
  robotSerialNumberPlaceholder: string;
  robotSerialNumberRequiredError: string;

  robotDescriptionLabel: string;
  robotDescriptionPlaceholder: string;

  robotTypeLabel: string;
  robotTypePlaceholder: string;
  robotTypeRequiredError: string;

  submitButtonLabel: string;
  robotCreatedMessage: string;
}

@Component({
  selector: 'app-create-robot',
  templateUrl: './create-robot.component.html',
  styleUrls: ['./create-robot.component.css'],
})
export class CreateRobotComponent {

  @Input() props: CreateRobotProps = content.components.robot || this.getDefaultProps();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  robotForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(private robotService: RobotService) {
    this.robotForm = new FormGroup({
      code: new FormControl('', [Validators.required, Validators.maxLength(30), Validators.pattern(/^[a-zA-Z0-9]+$/)]),
      nickname: new FormControl('', [Validators.required, Validators.maxLength(30), Validators.pattern(/^[a-zA-Z0-9]+$/)]),
      serialNumber: new FormControl('', [Validators.required, Validators.maxLength(30), Validators.pattern(/^[a-zA-Z0-9\s]*$/)]),
      description: new FormControl('', [Validators.maxLength(250)]),
      type: new FormControl('', [Validators.required, Validators.maxLength(25), Validators.pattern(/^[a-zA-Z0-9\s]*$/)]),
    });
  }

  getDefaultProps(): CreateRobotProps {
    return {
      robotCodeLabel: 'Robot Code',
      robotCodePlaceholder: 'Enter Robot Code',
      robotCodeRequiredError: 'Robot Code is required',

      robotNicknameLabel: 'Robot Nickname',
      robotNicknamePlaceholder: 'Enter Robot Nickname',
      robotNicknameRequiredError: 'Robot Nickname is required',

      robotSerialNumberLabel: 'Robot Serial Number',
      robotSerialNumberPlaceholder: 'Enter Robot Serial Number',
      robotSerialNumberRequiredError: 'Robot Serial Number is required',

      robotDescriptionLabel: 'Robot Description',
      robotDescriptionPlaceholder: 'Enter Robot Description',

      robotTypeLabel: 'Robot Type Name',
      robotTypePlaceholder: 'Enter Robot Type Name',
      robotTypeRequiredError: 'Robot Type is required',

      submitButtonLabel: 'Create Robot',
      robotCreatedMessage: 'Robot created successfully',
    };
  }

  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;
    const CreateRobotData: CreateRobotData = this.robotForm.value;

    this.robotService.createRobot(CreateRobotData).subscribe({
      next: () => {
        this.submitSuccessMessage = this.props.robotCreatedMessage;
        this.isLoading = false;
        this.robotForm.reset();
      },
      error: (error) => {
        console.error('Error creating robot:', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
