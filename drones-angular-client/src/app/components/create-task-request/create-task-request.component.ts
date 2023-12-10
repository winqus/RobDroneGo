import { Component, OnInit } from '@angular/core';
import { AbstractControl, FormBuilder, FormControl, FormGroup, ValidationErrors, ValidatorFn, Validators } from '@angular/forms';
import { User } from 'src/app/core/authentication/models/user.model';
import { UserService } from 'src/app/core/authentication/services/user.service';
import Building from 'src/app/core/models/building.model';
import { DeliveryTask } from 'src/app/core/models/deliveryTask.model';
import Floor from 'src/app/core/models/floor.model';
import Room from 'src/app/core/models/room.model';
import { SurveillanceTask } from 'src/app/core/models/surveillanceTask.model';
import { TaskRequest } from 'src/app/core/models/taskRequest.model';
import BuildingService from 'src/app/services/building.service';
import { FloorService } from 'src/app/services/floor.service';
import { RoomService } from 'src/app/services/room.service';
import { CreateTaskRequestDTO, TaskRequestService } from 'src/app/services/task-request.service';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

@Component({
  selector: 'app-create-task-request',
  templateUrl: './create-task-request.component.html',
  styleUrls: ['./create-task-request.component.css'],
})
export class CreateTaskRequestComponent implements OnInit {
  taskType: 'Delivery' | 'Surveillance' | '' = '';

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;

  buildings: Building[] = [];
  userData!: User;

  deliveryForm: FormGroup;
  pickupFloors: Floor[] = [];
  pickupRooms: Room[] = [];
  rooms: Room[] = [];
  deliveryFloors: Floor[] = [];
  deliveryRooms: Room[] = [];

  surveillanceForm: FormGroup;
  surveillanceFloors: Floor[] = [];

  constructor(
    private userService: UserService,
    private buildingService: BuildingService,
    private floorService: FloorService,
    private roomService: RoomService,
    private taskRequestService: TaskRequestService,
  ) {
    this.deliveryForm = new FormGroup(
      {
        pickUpBuildingCode: new FormControl('Building', Validators.required),
        pickUpFloorNumber: new FormControl('Floor', [Validators.pattern('^(?!Floor$).+'), Validators.required]),
        pickUpRoomId: new FormControl({ value: 'Room', disabled: true }, [Validators.pattern('^(?!Room$).+'), Validators.required]),
        deliveryBuildingCode: new FormControl('Building', Validators.required),
        deliveryFloorNumber: new FormControl('Floor', [Validators.pattern('^(?!Floor$).+'), Validators.required]),
        deliveryRoomId: new FormControl({ value: 'Room', disabled: true }, [Validators.pattern('^(?!Room$).+'), Validators.required]),
        pickUpContact: new FormControl('', [Validators.required, Validators.pattern('^[0-9]{9}$')]),
        pickUpName: new FormControl('', Validators.required),
        deliveryContact: new FormControl('', [Validators.required, Validators.pattern('^[0-9]{9}$')]),
        deliveryName: new FormControl('', Validators.required),
        confirmationCode: new FormControl('', [Validators.required, Validators.pattern('^[0-9]{4,6}$')]),
        description: new FormControl('', [Validators.required, Validators.pattern('[a-zA-Z0-9 ]+'), Validators.maxLength(1000)]),
      },
      { validators: this.roomMatchValidator },
    );

    this.surveillanceForm = new FormGroup({
      buildingCode: new FormControl('Building', [Validators.pattern('^(?!Building$).+'), Validators.required]),
      floorNumber: new FormControl([], Validators.required),
      contactNumber: new FormControl('', [Validators.required, Validators.pattern('^[0-9]{9}$')]),
    });
  }

  roomMatchValidator: ValidatorFn = (control: AbstractControl): ValidationErrors | null => {
    const pickUpRoomId = control.get('pickUpRoomId')?.value;
    const deliveryRoomId = control.get('deliveryRoomId')?.value;

    if (!pickUpRoomId || !deliveryRoomId) {
      return null;
    }

    if (pickUpRoomId === deliveryRoomId) {
      control.get('deliveryRoomId')?.setErrors({ sameRoom: true });
      return { sameRoom: true };
    }

    return null;
  };

  ngOnInit() {
    this.loadInitialData();
  }

  loadInitialData() {
    this.buildingService.getAllBuildings().subscribe((buildings) => (this.buildings = buildings.sort((a, b) => a.code.localeCompare(b.code))));
    this.userService.getCurrentUser().subscribe(({ user }) => {
      this.userData = user;
      this.deliveryForm.get('pickUpContact')?.patchValue(+user.phonenumber || '');
      this.deliveryForm.get('pickUpName')?.patchValue(user.firstName + ' ' + user.lastName);
      this.surveillanceForm.get('contactNumber')?.patchValue(+user.phonenumber || '');
    });
    this.roomService.getAllRooms().subscribe((rooms) => (this.rooms = rooms.sort((a, b) => a.name.localeCompare(b.name))));
  }

  onPickupBuildingChange(eventTarget: EventTarget | null) {
    const selectElement = eventTarget as HTMLSelectElement;
    const buildingCode = selectElement.value;
    this.floorService.getFloorsByBuildingCode(buildingCode).subscribe((floors) => (this.pickupFloors = floors.sort()));
    this.pickupRooms = [];
    this.deliveryForm.get('pickUpFloorNumber')?.patchValue('Floor');
    this.deliveryForm.get('pickUpFloorNumber')?.enable();
    this.deliveryForm.get('pickUpRoomId')?.disable();
    this.deliveryForm.get('pickUpRoomId')?.patchValue('Room');
    this.deliveryForm.get('pickUpRoomId')?.markAsPristine();
  }

  onPickupFloorChange(eventTarget: EventTarget | null) {
    const selectElement = eventTarget as HTMLSelectElement;
    const floorId = selectElement.value;
    this.pickupRooms = this.rooms.filter((room) => room.floorId === floorId);
    this.deliveryForm.get('pickUpRoomId')?.enable();
    this.deliveryForm.get('pickUpRoomId')?.patchValue('Room');
  }

  onDeliveryBuildingChange(eventTarget: EventTarget | null) {
    const selectElement = eventTarget as HTMLSelectElement;
    const buildingCode = selectElement.value;
    this.floorService.getFloorsByBuildingCode(buildingCode).subscribe((floors) => (this.deliveryFloors = floors.sort()));
    this.deliveryRooms = [];
    this.deliveryForm.get('deliveryFloorNumber')?.patchValue('Floor');
    this.deliveryForm.get('deliveryFloorNumber')?.enable();
    this.deliveryForm.get('deliveryRoomId')?.disable();
    this.deliveryForm.get('deliveryRoomId')?.patchValue('Room');
  }

  onDeliveryFloorChange(eventTarget: EventTarget | null) {
    const selectElement = eventTarget as HTMLSelectElement;
    const floorId = selectElement.value;
    this.deliveryRooms = this.rooms.filter((room) => room.floorId === floorId);
    this.deliveryForm.get('deliveryRoomId')?.enable();
    this.deliveryForm.get('deliveryRoomId')?.patchValue('Room');
    this.deliveryForm.get('deliveryRoomId')?.markAsPristine();
  }

  onSurveillanceBuildingChange(eventTarget: EventTarget | null) {
    const selectElement = eventTarget as HTMLSelectElement;
    const buildingCode = selectElement.value;
    this.floorService.getFloorsByBuildingCode(buildingCode).subscribe((floors) => (this.surveillanceFloors = floors.sort()));
    this.surveillanceForm.get('floorNumber')?.patchValue([]);
  }

  onSurveillanceFloorSelect(floorNumber: number, eventTarget: EventTarget | null) {
    const isChecked = (eventTarget as HTMLInputElement).checked;
    const selectedFloors = this.surveillanceForm.get('floorNumber')?.value as number[];

    if (isChecked) {
      if (!selectedFloors.includes(floorNumber)) {
        selectedFloors.push(floorNumber);
      }
    } else {
      const index = selectedFloors.indexOf(floorNumber);
      if (index > -1) {
        selectedFloors.splice(index, 1);
      }
    }

    this.surveillanceForm.get('floorNumber')?.setValue(selectedFloors.sort());
  }

  onTaskTypeChange(): void {
    this.submitSuccessMessage = null;
    this.errorResponse = [];

    this.resetForms();
  }

  resetForms() {
    this.deliveryForm.reset();
    this.surveillanceForm.reset();

    this.loadInitialData();
    this.clearForms();
  }

  clearForms() {
    this.deliveryForm.get('pickUpBuildingCode')?.patchValue('Building');
    this.deliveryForm.get('pickUpFloorNumber')?.disable();
    this.deliveryForm.get('pickUpRoomId')?.disable();
    this.deliveryForm.get('pickUpFloorNumber')?.patchValue('Floor');
    this.deliveryForm.get('pickUpRoomId')?.patchValue('Room');
    this.deliveryForm.get('deliveryBuildingCode')?.patchValue('Building');
    this.deliveryForm.get('deliveryFloorNumber')?.patchValue('Floor');
    this.deliveryForm.get('deliveryRoomId')?.patchValue('Room');
    this.deliveryForm.get('deliveryFloorNumber')?.disable();
    this.deliveryForm.get('deliveryRoomId')?.disable();

    this.surveillanceFloors = [];
    this.surveillanceForm.get('buildingCode')?.patchValue('Building');
  }

  submitTask() {
    this.isLoading = true;
    this.errorResponse = [];
    this.submitSuccessMessage = null;

    const taskRequest: Partial<CreateTaskRequestDTO> = {
      requesterEmail: this.userData.email,
    };

    if (this.taskType === 'Delivery' && this.deliveryForm.valid) {
      const form = this.deliveryForm.value;

      const deliveryTask: DeliveryTask = {
        pickUpRoomId: form.pickUpRoomId,
        deliveryRoomId: form.deliveryRoomId,
        pickUpContact: form.pickUpContact,
        pickUpName: form.pickUpName,
        deliveryContact: form.deliveryContact,
        deliveryName: form.deliveryName,
        confirmationCode: form.confirmationCode,
        description: form.description,
      };

      taskRequest.task = deliveryTask;
    } else if (this.taskType === 'Surveillance' && this.surveillanceForm.valid) {
      const form = this.surveillanceForm.value;

      const surveillanceTask: SurveillanceTask = {
        buildingCode: form.buildingCode,
        floorNumber: form.floorNumber,
        contactNumber: form.contactNumber,
      };

      taskRequest.task = surveillanceTask;
    }

    this.taskRequestService.createTaskRequest(taskRequest as CreateTaskRequestDTO).subscribe({
      next: (_response) => {
        this.submitSuccessMessage = 'Task request submitted successfully!';
        setTimeout(() => {
          this.isLoading = false;
          this.resetForms();
        }, 1000);
      },
      error: (err) => {
        console.error(err);
        this.errorResponse = err.error;
        this.isLoading = false;
      },
    });
  }
}
