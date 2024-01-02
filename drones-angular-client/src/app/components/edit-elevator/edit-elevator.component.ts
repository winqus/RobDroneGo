import { Component, EventEmitter, Input, OnChanges, OnInit, Output } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import Elevator from '../../core/models/elevator.model';
import { ElevatorService } from '../../services/elevator.service';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface EditElevatorProps {
  elevator: Elevator;
  editElevatorButtonLabel: string;
  elevatorEditedMessage: string;
}

interface UpdateElevatorData {
  buildingCode: string;
  make: string;
  model: string;
  serialNumber: string;
  description: string;
}

export const haveModelOrMakeValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
  const make = control.get('make');
  const model = control.get('model');

  if (make && model) {
    if (make.value !== '' && model.value === '') {
      model.setErrors({ modelRequired: true });
      return { modelRequired: true };
    }
    if (make.value === '' && model.value !== '') {
      make.setErrors({ makeRequired: true });
      return { makeRequired: true };
    }
  }
  make?.setErrors(null);
  model?.setErrors(null);
  return null;
};
@Component({
  selector: 'app-edit-elevator',
  templateUrl: './edit-elevator.component.html',
  styleUrls: ['./edit-elevator.component.css'],
})
export class EditElevatorComponent implements OnChanges, OnInit {
  @Input() props: EditElevatorProps = this.getDefaultProps();

  @Output() submitEvent = new EventEmitter<unknown>();
  elevators: any;
  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  elevatorForm: FormGroup;
  validationErrors = content.validation_errors;
  elevatorData?: Elevator;

  constructor(
    private elevatorService: ElevatorService,
    private route: ActivatedRoute,
  ) {
    this.elevatorForm = new FormGroup({
      buildingCode: new FormControl({ value: '', disabled: true }),
      make: new FormControl('', [Validators.maxLength(50), Validators.pattern(/^[a-zA-Z0-9 ]*$/)]),
      model: new FormControl('', [Validators.maxLength(50), Validators.pattern(/^[a-zA-Z0-9 ]*$/)]),
      serialNumber: new FormControl('', [Validators.maxLength(50), Validators.pattern(/^[a-zA-Z0-9 ]*$/)]),
      description: new FormControl('', [Validators.maxLength(250)]),
    });
  }

  getDefaultProps(): EditElevatorProps {
    return {
      elevator: {
        id: 'id',
        make: '',
        model: '',
        serialNumber: '',
        description: '',
        number: 0,
        buildingCode: '',
      },
      editElevatorButtonLabel: 'Update Elevator',
      elevatorEditedMessage: 'Elevator Edited',
    };
  }

  ngOnChanges() {
    if (this.props.elevator) {
      this.updateFormValues(this.props.elevator);
      this.elevatorForm.patchValue({
        buildingCode: this.props.elevator.buildingCode,
      });
    }
  }

  ngOnInit(): void {
    this.route.paramMap.subscribe((params) => {
      this.elevatorData = history.state.data || this.getDefaultProps().elevator;

      if (this.elevatorData) {
        this.updateFormValues(this.elevatorData);
      } else {
        console.error('Elevator data is null or undefined');
      }
    });
  }

  private updateFormValues(elevator: Elevator): void {
    this.elevatorForm.patchValue({
      buildingCode: elevator.buildingCode,
      make: elevator.make,
      model: elevator.model,
      serialNumber: elevator.serialNumber,
      description: elevator.description,
    });
  }

  onSubmit() {
    if (!this.elevatorData) {
      this.errorResponse = {
        error: {
          message: 'No elevator data',
        },
      };
      return;
    }

    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;

    const elevatorFormData: UpdateElevatorData = {
      buildingCode: this.elevatorForm.value.buildingCode,
      make: this.elevatorForm.value.make || '',
      model: this.elevatorForm.value.model || '',
      serialNumber: this.elevatorForm.value.serialNumber || '',
      description: this.elevatorForm.value.description || '',
    };

    this.elevatorService.updateElevator(this.elevatorData.buildingCode, elevatorFormData).subscribe({
      next: (elevator) => {
        this.submitSuccessMessage = this.props.elevatorEditedMessage;
        this.isLoading = false;
      },
      error: (error) => {
        console.error('elevator update error', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
