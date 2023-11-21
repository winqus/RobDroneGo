import { Component, Input, OnInit } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import Building from 'src/app/core/models/building.model';
import Floor from 'src/app/core/models/floor.model';
import BuildingService from 'src/app/services/building.service';
import { CreateElevatorData, ElevatorService } from 'src/app/services/elevator.service';
import { FloorService } from 'src/app/services/floor.service';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface CreateElevatorProps {
  buildingCodeLabel: string;
  buildingCodeLabelPlaceholder: string;
  buildingCodeLabelRequiredError: string;

  selectedFloorsLabel: string;
  selectedFloorsLabelPlaceholder: string;
  selectedFloorsLabelRequiredError: string;

  numberLabel: string;
  numberLabelPlaceholder: string;
  numberLabelRequiredError: string;

  makeLabel: string;
  makeLabelPlaceholder: string;
  makeLabelRequiredError: string;

  modelLabel: string;
  modelLabelPlaceholder: string;
  modelLabelRequiredError: string;

  serialNumberLabel: string;
  serialNumberLabelPlaceholder: string;

  descriptionLabel: string;
  descriptionLabelPlaceholder: string;

  submitButtonLabel: string;
  elevatorCreatedMessage: string;
}

export function atLeastOneFloorValidator(floors: number[]): ValidatorFn {
  return (): { [key: string]: boolean } | null => {
    if (floors.length > 0) return null;
    return { atLeastOneTaskRequired: true };
  };
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
  selector: 'app-create-elevator',
  templateUrl: './create-elevator.component.html',
  styleUrls: ['./create-elevator.component.css'],
})
export class CreateElevatorComponent implements OnInit {
  @Input() props: CreateElevatorProps = this.getDefaultProps();

  buildings: Building[] = [];
  floors: Floor[] = [];
  floorsSelected: number[] = [];

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;

  elevatorForm: FormGroup;

  constructor(
    private elevatorService: ElevatorService,
    private buildingService: BuildingService,
    private floorService: FloorService,
  ) {
    this.elevatorForm = new FormGroup(
      {
        buildingCode: new FormControl('', [Validators.required]),
        selectedFloors: new FormControl([]),
        number: new FormControl('', [Validators.required, Validators.pattern('[1-9][0-9]*')]),
        make: new FormControl('', [Validators.maxLength(50), Validators.pattern(/^[a-zA-Z0-9 ]*$/)]),
        model: new FormControl('', [Validators.maxLength(50), Validators.pattern(/^[a-zA-Z0-9 ]*$/)]),
        serialNumber: new FormControl('', [Validators.maxLength(50), Validators.pattern(/^[a-zA-Z0-9 ]*$/)]),
        description: new FormControl('', [Validators.maxLength(250)]),
      },
      { validators: haveModelOrMakeValidator },
    );
  }

  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildings: Building[]) => {
      this.buildings = buildings;
    });

    this.elevatorForm.get('buildingCode')?.valueChanges.subscribe((value) => {
      if (value !== null) {
        this.loadFloors(value);
      }
    });
  }

  handleCheckbox(event: any) {
    if (event.target.checked) {
      this.floorsSelected.push(event.target.value);
      const selectedFloorsControl = this.elevatorForm.get('selectedFloors');
      if (selectedFloorsControl !== null) {
        selectedFloorsControl.setErrors(atLeastOneFloorValidator(this.floorsSelected)(selectedFloorsControl));
      }
    } else {
      this.floorsSelected = this.floorsSelected.filter((floorNumber) => floorNumber !== event.target.value);
      const selectedFloorsControl = this.elevatorForm.get('selectedFloors');
      if (selectedFloorsControl !== null && this.floorsSelected.length === 0) {
        selectedFloorsControl.setErrors(atLeastOneFloorValidator(this.floorsSelected)(selectedFloorsControl));
      }
    }
  }

  getDefaultProps(): CreateElevatorProps {
    return {
      buildingCodeLabel: 'Building Code',
      buildingCodeLabelPlaceholder: 'Enter Building Code',
      buildingCodeLabelRequiredError: 'Building Code is required',

      selectedFloorsLabel: 'Selected Floors',
      selectedFloorsLabelPlaceholder: 'Select Floors',
      selectedFloorsLabelRequiredError: 'Selected Floors is required',

      numberLabel: 'Number',
      numberLabelPlaceholder: 'Enter Number',
      numberLabelRequiredError: 'Number is required',

      makeLabel: 'Make',
      makeLabelPlaceholder: 'Enter Make',
      makeLabelRequiredError: 'Make is required',

      modelLabel: 'Model',
      modelLabelPlaceholder: 'Enter Model',
      modelLabelRequiredError: 'Model is required',

      serialNumberLabel: 'Serial Number',
      serialNumberLabelPlaceholder: 'Enter Serial Number',

      descriptionLabel: 'Description',
      descriptionLabelPlaceholder: 'Enter Description',

      submitButtonLabel: 'Create Elevator',
      elevatorCreatedMessage: 'Elevator Created Successfully',
    };
  }

  loadFloors(buildingCode: string): void {
    this.floorService.getFloorsByBuildingCode(buildingCode).subscribe((floors: Floor[]) => {
      this.floors = floors;

      this.clearFloors();

      // Create form controls for each floor
      floors.forEach((floor) => {
        this.elevatorForm.addControl(`floor_${floor.floorNumber}`, new FormControl(false));
      });
    });
  }

  clearFloors(): void {
    Object.keys(this.elevatorForm.controls).forEach((key) => {
      if (key.startsWith('floor_')) {
        this.elevatorForm.removeControl(key);
      }
    });
  }

  onSubmit(): void {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;

    const buildingCode = this.elevatorForm.get('buildingCode')?.value;
    const elevatorData: CreateElevatorData = {
      number: this.elevatorForm.get('number')?.value,
      floors: this.floorsSelected,
    };

    if (this.elevatorForm.get('make')?.value) {
      elevatorData.make = this.elevatorForm.get('make')?.value;
    }
    if (this.elevatorForm.get('model')?.value) {
      elevatorData.model = this.elevatorForm.get('model')?.value;
    }
    if (this.elevatorForm.get('serialNumber')?.value) {
      elevatorData.serialNumber = this.elevatorForm.get('serialNumber')?.value;
    }
    if (this.elevatorForm.get('description')?.value) {
      elevatorData.description = this.elevatorForm.get('description')?.value;
    }

    this.elevatorService.createElevator(buildingCode, elevatorData).subscribe({
      next: () => {
        this.submitSuccessMessage = this.props.elevatorCreatedMessage;
        this.isLoading = false;
        this.floors = [];
        this.clearFloors();
        this.elevatorForm.reset();
        this.floorsSelected = [];
      },
      error: (error) => {
        console.error('Error creating elevator:', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
