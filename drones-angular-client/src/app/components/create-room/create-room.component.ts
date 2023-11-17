import { Component, Input } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import Room from 'src/app/core/models/room.model';
import { RoomService } from 'src/app/services/room.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
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
  @Input() props: CreateRoomProps = this.getDefaultProps();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  roomForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(private roomService: RoomService) {
    this.roomForm = new FormGroup({
      name: new FormControl('', [Validators.required, Validators.maxLength(this.props.roomNameMaxLength)]),
      description: new FormControl('', [Validators.maxLength(250)]),
      size: new FormGroup({
        width: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(1)]),
        length: new FormControl('', [Validators.required, Validators.pattern(/^\d+(\.\d+)?$/), Validators.min(1)]),
      }),
      position: new FormGroup({
        x: new FormControl('', [Validators.required]),
        y: new FormControl('', [Validators.required]),
      }),
      category: new FormControl('', Validators.required),
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
}
