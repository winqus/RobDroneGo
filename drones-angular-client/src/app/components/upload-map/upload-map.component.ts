import { Component, Input, OnInit } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import { at, map } from 'lodash';
import Building from '../../core/models/building.model';
import Floor from '../../core/models/floor.model';
import BuildingService from '../../services/building.service';
import { FloorService } from '../../services/floor.service';
import { MapService } from '../../services/map.service';

interface UploadMapComponentProps {
  buildingCodeLabel: string;
  buildingCodeRequiredError: string;

  floorNumberLabel: string;
  floorNumberRequiredError: string;

  fileLabel: string;
  fileInvalidError: string;

  jsonTextLabel: string;
  jsonTextPlaceholder: string;

  oneInputRequiredError: string;

  submitButtonLabel: string;
  submitSuccessMessage: string;
}

export function oneInputValidator(): ValidatorFn {
  return (control: AbstractControl): { [key: string]: boolean } | null => {
    const fileControl = control.get('file');
    const textAreaControl = control.get('jsonText');
    if (fileControl?.disabled || textAreaControl?.disabled) return null;
    return { requiredAtLeastOneInput: true };
  };
}

@Component({
  selector: 'app-upload-map',
  templateUrl: './upload-map.component.html',
  styleUrls: ['./upload-map.component.css'],
})
export class UploadMapComponent implements OnInit {
  @Input() props: UploadMapComponentProps = this.getDefaultProps();
  // validator for file upload and textarea
  jsonData: any = null; // Variable to store the JSON data
  fileErrorMessage: string | null = null;
  buildingCode: string | null = null;
  floorNumber: number | null = null;
  buildings: Building[] = [];
  floors: Floor[] = [];
  isLoading = false;
  submitSuccessMessage: string | null = null;
  errorResponse: any;

  uploadMapForm: FormGroup;

  constructor(
    private buildingService: BuildingService,
    private floorService: FloorService,
    private mapService: MapService,
  ) {
    this.uploadMapForm = new FormGroup(
      {
        buildingCode: new FormControl('', [Validators.required]),
        floorNumber: new FormControl({ value: '', disabled: true }, [Validators.required]),
        file: new FormControl(''),
        jsonText: new FormControl(''),
      },
      { validators: oneInputValidator() },
    );
  }

  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildings: Building[]) => {
      this.buildings = buildings;
    });
  }

  getDefaultProps(): UploadMapComponentProps {
    return {
      buildingCodeLabel: 'Building Code',
      buildingCodeRequiredError: 'Building code is required',

      floorNumberLabel: 'Floor Number',
      floorNumberRequiredError: 'Floor number is required',

      fileLabel: 'Upload Map File',
      fileInvalidError: 'Invalid JSON file',

      jsonTextLabel: 'JSON Text',
      jsonTextPlaceholder: 'Enter JSON text',

      oneInputRequiredError: 'At least one input is required',

      submitButtonLabel: 'Upload Map',
      submitSuccessMessage: 'Map uploaded successfully',
    };
  }

  // Function to handle file change event
  onFileChange(event: Event): void {
    console.log('onFileChange');
    this.fileErrorMessage = null; // Reset error message
    const input = event.target as HTMLInputElement;
    const file: File = (input.files as FileList)[0];
    const fileReader: FileReader = new FileReader();

    try {
      fileReader.onload = (e) => {
        try {
          this.jsonData = JSON.parse(fileReader.result as string);
        } catch (error) {
          this.fileErrorMessage = 'Invalid JSON file';
        }
      };
      this.uploadMapForm.get('jsonText')?.disable();
      fileReader.readAsText(file);
    } catch (error) {
      this.fileErrorMessage = 'Invalid JSON file';
    }
  }

  removeFile(): void {
    this.uploadMapForm.get('file')?.setValue('');
    this.jsonData = null;
    this.uploadMapForm.get('jsonText')?.enable();
    this.fileErrorMessage = null;
  }

  onTextAreaChange(event: Event): void {
    const value = (event.target as HTMLTextAreaElement).value;
    if (value === null) return;
    if (value.trim().length > 0) {
      this.uploadMapForm.get('file')?.disable();
    } else {
      this.uploadMapForm.get('file')?.enable();
    }
  }

  updateFloorList() {
    const buildingCode = this.uploadMapForm.get('buildingCode')!.value;
    this.floorService.getFloorsByBuildingCode(buildingCode).subscribe((floors: Floor[]) => {
      this.floors = floors;
    });
    if (this.floors) {
      this.uploadMapForm.get('floorNumber')!.enable();
    }
  }

  onSubmit(): void {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;

    const buildingCode = this.uploadMapForm.get('buildingCode')!.value;
    const floorNumber = this.uploadMapForm.get('floorNumber')!.value;
    let mapJson: any = null;
    if (this.jsonData) {
      mapJson = this.jsonData;
    } else {
      const jsonText = this.uploadMapForm.get('jsonText')!.value;
      try {
        mapJson = JSON.parse(jsonText);
      } catch (error) {
        this.errorResponse = { message: 'Invalid JSON text' };
        this.isLoading = false;
        return;
      }
    }
    this.mapService.uploadMap(buildingCode, floorNumber, mapJson).subscribe({
      next: () => {
        this.submitSuccessMessage = this.props.submitSuccessMessage;
        this.isLoading = false;
      },
      error: (error: any) => {
        console.error('room creation error', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
