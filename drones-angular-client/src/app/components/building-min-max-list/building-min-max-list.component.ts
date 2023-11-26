import { Component, EventEmitter, Input, Output } from '@angular/core';
import { AbstractControl, FormControl, FormGroup, ValidationErrors, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { forkJoin } from 'rxjs';
import Building from 'src/app/core/models/building.model';
import Floor from 'src/app/core/models/floor.model';
import BuildingService from 'src/app/services/building.service';
import { FloorService } from 'src/app/services/floor.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface BuildingMinMaxListProps {
  minFloorLabel: string;
  minFloorPlaceholder: string;
  minFloorRequiredError: string;

  maxFloorLabel: string;
  maxFloorPlaceholder: string;
  maxFloorRequiredError: string;

  listBuildingsButtonLabel: string;
  buildingsListedMessage: string;
}
@Component({
  selector: 'app-building-min-max-list',
  templateUrl: './building-min-max-list.component.html',
  styleUrls: ['./building-min-max-list.component.css'],
})
export class BuildingMinMaxListComponent {
  buildings: Building[] = [];
  floors: Floor[] = [];
  @Input() props: BuildingMinMaxListProps = this.getDefaultProps();
  @Output() submitEvent = new EventEmitter<unknown>();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  noBuildingsFoundMessage: string = '';
  isLoading = false;
  loadedOnce = false;
  buildingMinMaxForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(
    private buildingService: BuildingService,
    private floorService: FloorService,
    private route: ActivatedRoute,
    private router: Router,
  ) {
    this.buildingMinMaxForm = new FormGroup(
      {
        minFloor: new FormControl('', [Validators.required]),
        maxFloor: new FormControl('', [Validators.required]),
      },
      { validators: this.minMaxValidator },
    );
  }

  getDefaultProps(): BuildingMinMaxListProps {
    return {
      minFloorLabel: 'Minimum amount of floors',
      minFloorPlaceholder: 'Enter Min Floor',
      minFloorRequiredError: 'Min Floors is required',

      maxFloorLabel: 'Maximum amount of floors',
      maxFloorPlaceholder: 'Enter Max Floor',
      maxFloorRequiredError: 'Max Floors is required',

      listBuildingsButtonLabel: 'List Buildings',
      buildingsListedMessage: 'Buildings listed succesfully',
    };
  }

  minMaxValidator(control: AbstractControl): ValidationErrors | null {
    const minFloor = control.get('minFloor');
    const maxFloor = control.get('maxFloor');

    if (!minFloor || !maxFloor || minFloor.value < 0 || maxFloor.value < 0) {
      return { nonNegative: true };
    }

    return minFloor.value <= maxFloor.value ? null : { minMax: true };
  }

  getNumberOfFloors(buildingCode: Building['code'], floors: Floor[]): number {
    return floors.filter((floor) => floor.buildingCode === buildingCode).length;
  }

  getBuildings(minFloor: number, maxFloor: number) {
    this.floorService.getAllFloors().subscribe({
      next: (floors: Floor[]) => {
        this.floors = floors;
        const uniqueBuildingCodes = new Set(floors.map((floor) => floor.buildingCode));
        const buildingObservables = Array.from(uniqueBuildingCodes).map((code) => this.buildingService.getBuildingByCode(code));

        forkJoin(buildingObservables).subscribe({
          next: (buildings: Building[]) => {
            // Filter buildings based on the count of floors in the specified range
            this.buildings = buildings.filter((building) => {
              const numberOfFloors = this.getNumberOfFloors(building.code, floors);
              return numberOfFloors >= minFloor && numberOfFloors <= maxFloor;
            });

            if (this.buildings.length === 0) {
              this.noBuildingsFoundMessage = 'No buildings found within the given range';
            } else {
              this.submitSuccessMessage = this.props.buildingsListedMessage;
            }
            this.isLoading = false;
            this.loadedOnce = true;
          },
          error: (error: any) => {
            console.error('Error getting building details', error);
            this.errorResponse = error;
            this.isLoading = false;
          },
        });
      },
      error: (error: any) => {
        console.error('Error getting floors', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }

  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;
    this.noBuildingsFoundMessage = '';

    const minFloor = this.buildingMinMaxForm.value.minFloor;
    const maxFloor = this.buildingMinMaxForm.value.maxFloor;

    this.getBuildings(minFloor, maxFloor);
  }
}
