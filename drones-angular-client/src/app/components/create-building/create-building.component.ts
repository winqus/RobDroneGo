import { Component, Input } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import BuildingService, { CreateBuildingData } from '../../services/building.service';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface CreateBuildingProps {
  buildingNameLabel: string;
  buildingNamePlaceholder: string;

  buildingCodeLabel: string;
  buildingCodePlaceholder: string;
  buildingCodeRequiredError: string;

  buildingDescriptionLabel: string;
  buildingDescriptionPlaceholder: string;

  floorSizeLengthLabel: string;
  floorSizeLengthPlaceholder: string;
  floorSizeLengthRequiredError: string;

  floorSizeWidthLabel: string;
  floorSizeWidthPlaceholder: string;
  floorSizeWidthRequiredError: string;

  submitButtonLabel: string;
  buildingCreatedMessage: string;
}

@Component({
  selector: 'app-create-building',
  templateUrl: './create-building.component.html',
  styleUrls: ['./create-building.component.css'],
})
export class CreateBuildingComponent {
  @Input() props: CreateBuildingProps = content.components.building || this.getDefaultProps();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  buildingForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(private buildingService: BuildingService) {
    this.buildingForm = new FormGroup({
      code: new FormControl('', [Validators.required, Validators.maxLength(5), Validators.pattern(/^[a-zA-Z0-9]+$/)]),
      name: new FormControl('', [Validators.maxLength(50), Validators.pattern(/^[a-zA-Z0-9 ]*$/)]),
      description: new FormControl('', [Validators.maxLength(255)]),
      floorSizeLength: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(1)]),
      floorSizeWidth: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(1)]),
    });
  }

  getDefaultProps(): CreateBuildingProps {
    return {
      buildingNameLabel: 'Building Name',
      buildingNamePlaceholder: 'Enter Building Name',
      buildingCodeLabel: 'Building Code',
      buildingCodePlaceholder: 'Enter Building Code',
      buildingCodeRequiredError: 'Building Code is required',
      buildingDescriptionLabel: 'Building Description',
      buildingDescriptionPlaceholder: 'Enter Building Description',
      floorSizeLengthLabel: 'Floor Size Length',
      floorSizeLengthPlaceholder: 'Enter Floor Size Length',
      floorSizeLengthRequiredError: 'Floor Size Length is required',
      floorSizeWidthLabel: 'Floor Size Width',
      floorSizeWidthPlaceholder: 'Enter Floor Size Width',
      floorSizeWidthRequiredError: 'Floor Size Width is required',
      submitButtonLabel: 'Create Building',
      buildingCreatedMessage: 'Building Created',
    };
  }

  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;
    const createBuildingData: CreateBuildingData = this.buildingForm.value;
    console.log(createBuildingData);

    this.buildingService.createBuilding(createBuildingData).subscribe({
      next: () => {
        this.submitSuccessMessage = this.props.buildingCreatedMessage;
        this.isLoading = false;
      },
      error: (error) => {
        console.error('building creation error', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
