import { Component, Input } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import Building from '../../core/models/building.model';
import Floor from '../../core/models/floor.model';
import Room from '../../core/models/room.model';
import BuildingService from '../../services/building.service';
import { FloorService } from '../../services/floor.service';
import { RoomService } from '../../services/room.service';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface CreateRoomProps {
  roomNameLabel: string;
  roomNamePlaceholder: string;
  roomNameMaxLength: number;
  roomNameRequiredError: string;

  roomDescriptionLabel: string;
  roomDescriptionPlaceholder: string;

  roomSizeLabel: string;
  roomSizePlaceholder: string;
  roomSizeRequiredError: string;

  roomSizeWLabel: string;
  roomSizeWPlaceholder: string;
  roomSizeWRequiredError: string;

  roomSizeLLabel: string;
  roomSizeLPlaceholder: string;
  roomSizeLRequiredError: string;

  roomPositionLabel: string;
  roomPositionPlaceholder: string;
  roomPositionRequiredError: string;

  roomPositionXLabel: string;
  roomPositionXPlaceholder: string;
  roomPositionXRequiredError: string;

  roomPositionYLabel: string;
  roomPositionYPlaceholder: string;
  roomPositionYRequiredError: string;

  roomCategoryLabel: string;
  roomCategoryPlaceholder: string;
  roomCategoryOptions: string[];

  submitButtonLabel: string;
  roomCreatedMessage: string;
}

@Component({
  selector: 'app-create-room',
  templateUrl: './create-room.component.html',
  styleUrls: ['./create-room.component.css'],
})
export class CreateRoomComponent {
  floors: Floor[] = [];
  buildings: Building[] = [];

  @Input() props: CreateRoomProps = this.getDefaultProps();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  roomForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(
    private roomService: RoomService,
    private floorService: FloorService,
    private buildingService: BuildingService,
  ) {
    this.roomForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      floorNumber: new FormControl({ value: '', disabled: true }, [Validators.required, Validators.pattern(/^-?\d+$/)]),
      name: new FormControl('', [Validators.required, Validators.maxLength(this.props.roomNameMaxLength)]),
      description: null || new FormControl('', [Validators.maxLength(250)]),
      size: new FormGroup({
        width: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(1)]),
        length: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(1)]),
      }),
      position: new FormGroup({
        x: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(0)]),
        y: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(0)]),
      }),
      category: new FormControl('', Validators.required),
    });
  }

  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildings: Building[]) => {
      this.buildings = buildings;
    });
  }

  getDefaultProps(): CreateRoomProps {
    return {
      roomNameLabel: 'Room Name',
      roomNamePlaceholder: 'Enter Room Name',
      roomNameMaxLength: 50,
      roomNameRequiredError: 'Room Name is required',

      roomDescriptionLabel: 'Room Description',
      roomDescriptionPlaceholder: 'Enter Room Description',

      roomSizeLabel: 'Room Size',
      roomSizePlaceholder: 'Enter Room Size',
      roomSizeRequiredError: 'Room Size is required',

      roomSizeWLabel: 'Room Size Width',
      roomSizeWPlaceholder: 'Enter Room Width',
      roomSizeWRequiredError: 'Room Width is required',

      roomSizeLLabel: 'Room Size Length',
      roomSizeLPlaceholder: 'Enter Room Length',
      roomSizeLRequiredError: 'Room Length is required',

      roomPositionLabel: 'Room Position',
      roomPositionPlaceholder: 'Enter Room Position',
      roomPositionRequiredError: 'Room Position is required',

      roomPositionXLabel: 'Room Position X',
      roomPositionXPlaceholder: 'Enter Room Position X',
      roomPositionXRequiredError: 'Room Position X is required',

      roomPositionYLabel: 'Room Position Y',
      roomPositionYPlaceholder: 'Enter Room Position Y',
      roomPositionYRequiredError: 'Room Position Y is required',

      roomCategoryLabel: 'Room Category',
      roomCategoryPlaceholder: 'Enter Room Category',
      roomCategoryOptions: ['Office', 'Amphitheater', 'Laboratory', 'Other'],

      submitButtonLabel: 'Create Room',
      roomCreatedMessage: 'Room Created',
    };
  }

  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;
    const createRoomData: Room = this.roomForm.value;
    const floorId = this.floors.find((floor: Floor) => floor.floorNumber === +this.roomForm.value.floorNumber)?.id;
    createRoomData.floorId = floorId!;
    this.roomService.createRoom(createRoomData).subscribe({
      next: () => {
        this.submitSuccessMessage = this.props.roomCreatedMessage;
        this.isLoading = false;
      },
      error: (error: any) => {
        console.error('room creation error', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }

  updateFloorList() {
    const buildingCode = this.roomForm.get('buildingCode')!.value;
    this.floorService.getFloorsByBuildingCode(buildingCode).subscribe((floors: Floor[]) => {
      this.floors = floors;
    });
    if (this.floors) {
      this.roomForm.get('floorNumber')!.enable();
    }
  }
}
