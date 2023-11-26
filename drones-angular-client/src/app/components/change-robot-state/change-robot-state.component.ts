import { Component, EventEmitter, Input, OnChanges, Output } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import Robot from 'src/app/core/models/robot.model';
import { RobotService } from 'src/app/services/robot.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface ChangeRobotStateProps {
  robotCodeLabel: string;
  robotCodePlaceholder: string;
  robotCodeRequiredError: string;
  availableLabel: string;
  availableRequiredError: string;
  changeStateButtonLabel: string;
  stateChangedMessage: string;
}

@Component({
  selector: 'app-change-robot-state',
  templateUrl: './change-robot-state.component.html',
  styleUrls: ['./change-robot-state.component.css'],
})
export class ChangeRobotStateComponent {
  @Input() props: ChangeRobotStateProps = this.getDefaultProps();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  robotForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(
    private robotService: RobotService,
    private route: ActivatedRoute,
  ) {
    this.robotForm = new FormGroup({
      robotCode: new FormControl('', [Validators.required, Validators.maxLength(30), Validators.pattern(/^[a-zA-Z0-9]*$/)]),
      available: new FormControl(false, [Validators.required, Validators.pattern(/^(true|false)$/)]),
    });
  }

  getDefaultProps(): ChangeRobotStateProps {
    return {
      robotCodeLabel: 'Robot Code',
      robotCodePlaceholder: 'Enter Robot Code',
      robotCodeRequiredError: 'Robot Code is required',
      availableLabel: 'Available',
      availableRequiredError: 'Available is required',
      changeStateButtonLabel: 'Change Robot State',
      stateChangedMessage: 'Robot State Changed',
    };
  }

  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;

    const availableControl = this.robotForm.get('available');
    const robotCodeControl = this.robotForm.get('robotCode');

    if (availableControl && availableControl.valid && 
      robotCodeControl && robotCodeControl.valid) {

      const availableValue = availableControl.value === 'true';
      const robotCodeValue = robotCodeControl.value;

      this.robotService.changeRobotState(robotCodeValue, availableValue).subscribe({
        next: () => {
          this.submitSuccessMessage = this.props.stateChangedMessage;
          this.isLoading = false;
          this.robotForm.reset();
        },
        error: (error) => {
          console.error('Robot state change error', error);
          this.errorResponse = error;
          this.isLoading = false;
        },
      });
  }
  }
}

