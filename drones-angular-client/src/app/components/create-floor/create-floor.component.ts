import { Component, Input, OnInit } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import Building from 'src/app/core/models/building.model';
import BuildingService from 'src/app/services/building.service';
import { CreateFloorData, FloorService } from 'src/app/services/floor.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface CreateFloorProps {
  floorNumberLabel: string;
  floorNumberPlaceholder: string;
  floorNumberRequiredError: string;

  floorDescriptionLabel: string;
  floorDescriptionPlaceholder: string;

  submitButtonLabel: string;
  floorCreatedMessage: string;
}

@Component({
  selector: 'app-create-floor',
  templateUrl: './create-floor.component.html',
  styleUrls: ['./create-floor.component.css'],
})
export class CreateFloorComponent implements OnInit {
  buildings: Building[] = [];

  @Input() props: CreateFloorProps = content.components.floor || this.getDefaultProps();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  floorForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(
    private floorService: FloorService,
    private buildingService: BuildingService,
  ) {
    this.floorForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      floorNumber: new FormControl('', [Validators.required, Validators.pattern(/^-?\d+$/)]),
      description: new FormControl('', [Validators.maxLength(255)]),
    });
  }

  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildings: Building[]) => {
      this.buildings = buildings;
    });
  }

  getDefaultProps(): CreateFloorProps {
    return {
      floorNumberLabel: 'Floor Number',
      floorNumberPlaceholder: 'Enter Floor Number',
      floorNumberRequiredError: 'Floor Number is required',

      floorDescriptionLabel: 'Floor Description',
      floorDescriptionPlaceholder: 'Enter Floor Description',

      submitButtonLabel: 'Create Floor',
      floorCreatedMessage: 'Floor created successfully!',
    };
  }

  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;
    const createFloorData: CreateFloorData = this.floorForm.value;
    console.log(createFloorData);

    this.floorService.createFloor(createFloorData).subscribe({
      next: () => {
        this.submitSuccessMessage = this.props.floorCreatedMessage;
        this.isLoading = false;
        this.floorForm.reset();
      },
      error: (error) => {
        console.error('Error creating floor:', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
