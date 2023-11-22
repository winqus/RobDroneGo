import { Component, EventEmitter, Input, Output } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import Passage from 'src/app/core/models/passage.model';
import { PassageService } from 'src/app/services/passage.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface CreatePassageProps {
  buildingCode1: string;
  buildingCode2: string;
  floorNumber1: string;
  floorNumber2: string;
  buildingCode1Placeholder: string;
  buildingCode2Placeholder: string;
  floorNumber1Placeholder: string;
  floorNumber2Placeholder: string;
  createPassageButtonLabel: string;
  passageCreatedMessage: string;
}

@Component({
  selector: 'app-create-passage',
  templateUrl: './create-passage.component.html',
  styleUrls: ['./create-passage.component.css'],
})
export class CreatePassageComponent {
  @Input() props: CreatePassageProps = this.getDefaultProps();

  @Output() submitEvent = new EventEmitter<unknown>();

  errorResponse: any;
  isLoading = false;
  passageForm: FormGroup;
  validationErrors = content.validation_errors;
  submitSuccessMessage: SuccessMessage = null;

  constructor(private passageService: PassageService) {
    this.passageForm = new FormGroup({
      buildingCode1: new FormControl('', [Validators.required]),
      buildingCode2: new FormControl('', [Validators.required]),
      floorNumber1: new FormControl('', [Validators.required, Validators.pattern(/^[-]?\d+$/)]),
      floorNumber2: new FormControl('', [Validators.required, Validators.pattern(/^[-]?\d+$/)]),
    });
  }

  getDefaultProps(): CreatePassageProps {
    return {
      buildingCode1: 'Building 1 Code',
      buildingCode2: 'Building 2 Code',
      floorNumber1: 'Floor 1 Number',
      floorNumber2: 'Floor 2 Number',
      buildingCode1Placeholder: 'Enter Building 1 Code',
      buildingCode2Placeholder: 'Enter Building 2 Code',
      floorNumber1Placeholder: 'Enter Floor 1 Number',
      floorNumber2Placeholder: 'Enter Floor 2 Number',
      createPassageButtonLabel: 'Create Passage',
      passageCreatedMessage: 'Passage Created',
    };
  }

  onSubmit() {
    this.isLoading = true;
    const passageData: Passage = this.passageForm.value;

    this.passageService.createPassage(passageData).subscribe({
      next: (passage) => {
        this.submitSuccessMessage = this.props.passageCreatedMessage;

        this.isLoading = false;
      },
      error: (error) => {
        console.error('passage creation error', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
