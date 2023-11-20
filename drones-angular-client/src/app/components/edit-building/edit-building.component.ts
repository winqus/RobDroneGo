import { Component, EventEmitter, Input, OnChanges, OnInit, Output } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import Building from 'src/app/core/models/building.model';
import BuildingService from 'src/app/services/building.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface EditBuildingProps {
  building: Building;
  editBuildingButtonLabel: string;
  buildingEditedMessage: string;
}

interface UpdateBuildingData {
  name?: string;
  description?: string;
  floorSizeLength?: number;
  floorSizeWidth?: number;
}

@Component({
  selector: 'app-edit-building',
  templateUrl: './edit-building.component.html',
  styleUrls: ['./edit-building.component.css'],
})
export class EditBuildingComponent implements OnChanges, OnInit {
  @Input() props: EditBuildingProps = this.getDefaultProps();

  @Output() submitEvent = new EventEmitter<unknown>();
  buildings: any;
  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  buildingForm: FormGroup;
  validationErrors = content.validation_errors;
  buildingData?: Building;

  constructor(
    private buildingService: BuildingService,
    private route: ActivatedRoute,
  ) {
    this.buildingForm = new FormGroup({
      name: new FormControl('', [Validators.maxLength(50), Validators.pattern(/^[a-zA-Z0-9 ]*$/)]),
      description: new FormControl('', [Validators.maxLength(255)]),
      floorSizeLength: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(1)]),
      floorSizeWidth: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(1)]),
    });
  }

  getDefaultProps(): EditBuildingProps {
    return {
      building: {
        id: 'id',
        name: '',
        code: '',
        description: '',
        floorSizeLength: 0,
        floorSizeWidth: 0,
      },
      editBuildingButtonLabel: 'Update Building',
      buildingEditedMessage: 'Building Edited',
    };
  }
  ngOnChanges() {
    if (this.props.building) {
      this.buildingForm.patchValue({
        name: this.props.building.name,
        description: this.props.building.description,
        floorSizeLength: this.props.building.floorSizeLength,
        floorSizeWidth: this.props.building.floorSizeWidth,
      });
    }
  }
  ngOnInit(): void {
    this.route.paramMap.subscribe((params) => {
      this.buildingData = history.state.data || this.getDefaultProps().building;

      if (this.buildingData) {
        this.buildingForm.patchValue({
          id: this.buildingData.id,
          name: this.buildingData.name,
          description: this.buildingData.description,
          floorSizeLength: this.buildingData.floorSizeLength,
          floorSizeWidth: this.buildingData.floorSizeWidth,
        });
      } else {
        console.error('Building data is null or undefined');
      }
    });
  }
  onSubmit() {
    if (!this.buildingData) {
      this.errorResponse = {
        error: {
          message: 'No building data',
        },
      };
      return;
    }

    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;

    const buildingFormData: UpdateBuildingData = {
      name: this.buildingForm.value.name,
      description: this.buildingForm.value.description,
      floorSizeLength: this.buildingForm.value.floorSizeLength,
      floorSizeWidth: this.buildingForm.value.floorSizeWidth,
    };

    this.buildingService.updateBuilding(this.buildingData.id, buildingFormData).subscribe({
      next: (building) => {
        this.submitSuccessMessage = this.props.buildingEditedMessage;
        this.isLoading = false;
      },
      error: (error) => {
        console.error('building update error', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
