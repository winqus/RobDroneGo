import { Component, EventEmitter, Input, OnChanges, Output } from '@angular/core';
import { FormControl, FormGroup } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import Floor from '../../core/models/floor.model';
import { FloorService } from '../../services/floor.service';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface EditFloorProps {
  floor: Floor;
  editFloorButtonLabel: string;
  floorEditedMessage: string;
}
@Component({
  selector: 'app-edit-floor',
  templateUrl: './edit-floor.component.html',
  styleUrls: ['./edit-floor.component.css'],
})
export class EditFloorComponent implements OnChanges {
  @Input() props: EditFloorProps = this.getDefaultProps();

  @Output() submitEvent = new EventEmitter<unknown>();
  floors: any;
  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  isLoading = false;
  floorForm: FormGroup;
  validationErrors = content.validation_errors;
  floorData?: Floor;

  constructor(
    private floorService: FloorService,
    private route: ActivatedRoute,
  ) {
    this.floorForm = new FormGroup({
      floorNumber: new FormControl({ value: '', disabled: true }),
      description: new FormControl(''),
      buildingCode: new FormControl({ value: '', disabled: true }),
    });
  }

  getDefaultProps(): EditFloorProps {
    return {
      floor: {
        id: '',
        description: '',
        servedByElevator: false,
        floorNumber: 0,
        buildingCode: 'code',
      },
      editFloorButtonLabel: 'Update Floor',
      floorEditedMessage: 'Floor Edited',
    };
  }

  ngOnChanges() {
    if (this.props.floor) {
      this.floorForm.patchValue({
        buildingCode: this.props.floor.buildingCode,
        floorNumber: this.props.floor.floorNumber,
        description: this.props.floor.description,
      });
    }
  }
  ngOnInit(): void {
    this.route.paramMap.subscribe((params) => {
      const code = params.get('code');
      const number = params.get('number');

      this.floorData = history.state.data as Floor;

      this.floorForm.patchValue({
        buildingCode: code,
        floorNumber: number,
        description: this.floorData.description,
      });
    });
  }

  onSubmit() {
    if (!this.floorData) {
      this.errorResponse = {
        error: {
          message: 'No floor data',
        },
      };
      return;
    }

    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;
    const floorFormData: Floor = this.floorForm.value;
    console.log(floorFormData);

    this.floorService.updateFloor(this.floorData.id, floorFormData).subscribe({
      next: (floor) => {
        this.submitSuccessMessage = this.props.floorEditedMessage;
        this.isLoading = false;
      },
      error: (error) => {
        console.error('floor update error', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
