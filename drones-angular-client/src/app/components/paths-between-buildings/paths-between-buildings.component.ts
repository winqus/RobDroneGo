import { Component, OnInit } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import Building from '../../core/models/building.model';
import Floor from '../../core/models/floor.model';
import Room from '../../core/models/room.model';
import BuildingService from '../../services/building.service';
import { FloorService } from '../../services/floor.service';
import { PlanningService } from '../../services/planning.service';
import { RoomService } from '../../services/room.service';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

@Component({
  selector: 'app-paths-between-buildings',
  templateUrl: './paths-between-buildings.component.html',
  styleUrls: ['./paths-between-buildings.component.css'],
})
export class PathsBetweenBuildingsComponent implements OnInit {
  buildingsOrigin: Building[] = [];
  floorsOrigin: Floor[] = [];
  roomsOrigin: Room[] = [];

  buildingsDestination: Building[] = [];
  floorsDestination: Floor[] = [];
  roomsDestination: Room[] = [];

  pathForm: FormGroup;

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  calculationResult: any;
  loadedOnce = false;

  constructor(
    private buildingService: BuildingService,
    private planningService: PlanningService,
    private floorService: FloorService,
    private roomService: RoomService,
    private route: ActivatedRoute,
    private router: Router,
  ) {
    this.pathForm = new FormGroup({
      buildingCodeO: new FormControl('', Validators.required),
      floorNumberO: new FormControl({ value: '', disabled: true }, [Validators.required, Validators.pattern(/^-?\d+$/)]),
      roomO: new FormControl({ value: '', disabled: true }, Validators.required),
      buildingCodeD: new FormControl('', Validators.required),
      floorNumberD: new FormControl({ value: '', disabled: true }, [Validators.required, Validators.pattern(/^-?\d+$/)]),
      roomD: new FormControl({ value: '', disabled: true }, Validators.required),
    });
  }

  onSubmit() {
    this.errorResponse = [];
    if (this.pathForm.value.roomO === this.pathForm.value.roomD) {
      console.error('Origin and Destination rooms must be different.');
      this.errorResponse = 'Origin and Destination rooms must be different.';
      return;
    }

    const floorIdO = this.floorsOrigin.find((floor: Floor) => floor.floorNumber === +this.pathForm.value.floorNumberO)?.id;
    const floorIdD = this.floorsDestination.find((floor: Floor) => floor.floorNumber === +this.pathForm.value.floorNumberD)?.id;
    const data = {
      origin_building_code: this.pathForm.value.buildingCodeO,
      origin_floor_number: this.pathForm.value.floorNumberO,
      origin_room: this.pathForm.value.roomO,
      destination_building_code: this.pathForm.value.buildingCodeD,
      destination_floor_number: this.pathForm.value.floorNumberD,
      destination_room: this.pathForm.value.roomD,
      minimize_elevator_uses: true,
      minimize_building_count: false,
    };

    this.planningService.calculateCells(data).subscribe({
      next: (result) => {
        this.calculationResult = result;
        this.errorResponse = [];
      },
      error: (error) => {
        console.error('Calculation error:', error);
        this.calculationResult = null;
        this.errorResponse = error;
      },
    });
  }

  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildingsOrigin: Building[]) => {
      this.buildingsOrigin = buildingsOrigin.sort((building1, building2) => building1.code.localeCompare(building2.code));
    });

    this.buildingService.getAllBuildings().subscribe((buildingsDestination: Building[]) => {
      this.buildingsDestination = buildingsDestination.sort((building1, building2) => building1.code.localeCompare(building2.code));
    });
  }

  updateFloorListOrigin() {
    this.floorsOrigin = [];
    this.roomsOrigin = [];

    this.pathForm.patchValue({
      floorNumberO: '',
      roomO: '',
    });
    this.pathForm.markAsUntouched();

    const buildingCodeO = this.pathForm.get('buildingCodeO')!.value;
    this.floorService.getFloorsByBuildingCode(buildingCodeO).subscribe((floorsOrigin: Floor[]) => {
      this.floorsOrigin = floorsOrigin.sort((floor1, floor2) => floor1.floorNumber - floor2.floorNumber);
    });
    if (this.floorsOrigin) {
      this.pathForm.get('floorNumberO')!.enable();
    }
  }

  updateFloorListDestination() {
    if (!this.pathForm.get('buildingCodeD')!.value) {
      return;
    }

    this.floorsDestination = [];
    this.roomsDestination = [];

    this.pathForm.patchValue({
      floorNumberD: '',
      roomD: '',
    });
    this.pathForm.markAsUntouched();

    const buildingCodeD = this.pathForm.get('buildingCodeD')!.value;
    this.floorService.getFloorsByBuildingCode(buildingCodeD).subscribe((floorsDestination: Floor[]) => {
      this.floorsDestination = floorsDestination.sort((floor1, floor2) => floor1.floorNumber - floor2.floorNumber);
    });
    if (this.floorsDestination) {
      this.pathForm.get('floorNumberD')!.enable();
    }
  }

  updateRoomListOrigin() {
    const floorNumberO = this.pathForm.get('floorNumberO')!.value;
    const currentFloorO = this.floorsOrigin.find((floor: Floor) => floor.floorNumber == floorNumberO);

    this.roomService.getAllRooms().subscribe((roomsOrigin: Room[]) => {
      this.roomsOrigin = roomsOrigin?.filter((room: Room) => room.floorId == currentFloorO!.id).sort((room1, room2) => room1.name.localeCompare(room2.name));

      if (this.roomsOrigin) {
        this.pathForm.get('roomO')!.enable();

        this.pathForm.patchValue({
          roomO: this.roomsOrigin[0]?.id,
        });

        this.checkSameRooms();
      }
    });
  }

  updateRoomListDestination() {
    const floorNumberD = this.pathForm.get('floorNumberD')!.value;
    const currentFloorD = this.floorsDestination.find((floor: Floor) => floor.floorNumber == floorNumberD);

    this.roomService.getAllRooms().subscribe((roomsDestination: Room[]) => {
      this.roomsDestination = roomsDestination?.filter((room: Room) => room.floorId == currentFloorD!.id).sort((room1, room2) => room1.name.localeCompare(room2.name));

      if (this.roomsDestination) {
        this.pathForm.get('roomD')!.enable();

        this.pathForm.patchValue({
          roomD: this.roomsDestination[0]?.id,
        });

        this.checkSameRooms();
      }
    });
  }

  checkSameRooms() {
    const formRoomOriginId = this.pathForm.get('roomO')!.value;
    const formRoomDestinationId = this.pathForm.get('roomD')!.value;

    if (!formRoomOriginId || !formRoomDestinationId) {
      return;
    }

    if (formRoomOriginId === formRoomDestinationId) {
      this.pathForm.get('roomD')!.setErrors({ sameRoom: true });
    } else {
      this.pathForm.get('roomD')!.setErrors(null);
    }
  }
}
