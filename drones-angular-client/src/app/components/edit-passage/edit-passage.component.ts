import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import Passage from '../../core/models/passage.model';
import { PassageService } from '../../services/passage.service';
import { CreatePassageProps } from '../create-passage/create-passage.component';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface UpdatePassageProps {
  oldPassageSelectedRequiredError: string;
  buildingCode1: string;
  buildingCode2: string;
  floorNumber1: string;
  floorNumber2: string;
  buildingCode1Placeholder: string;
  buildingCode1RequiredError: string;
  buildingCode2Placeholder: string;
  buildingCode2RequiredError: string;
  floorNumber1Placeholder: string;
  floorNumber1RequiredError: string;
  floorNumber2Placeholder: string;
  floorNumber2RequiredError: string;
  updatePassageButtonLabel: string;
  updatePassageSuccessMessage: string;
}

@Component({
  selector: 'app-edit-passage',
  templateUrl: './edit-passage.component.html',
  styleUrls: ['./edit-passage.component.css'],
})
export class EditPassageComponent implements OnInit {
  @Input() props: UpdatePassageProps = this.getDefaultProps();

  @Output() submitEvent = new EventEmitter<unknown>();

  passages: Passage[] = [];
  errorResponse: any;
  isLoading = false;
  submitSuccessMessage: SuccessMessage = null;
  passageForm: FormGroup;
  selectedPassageId: string | null = null; // Track the selected passage ID

  constructor(private passageService: PassageService) {
    this.passageForm = new FormGroup({
      oldPassagePosition: new FormControl('', [Validators.required]),
      buildingCode1: new FormControl('', [Validators.required]),
      buildingCode2: new FormControl('', [Validators.required]),
      floorNumber1: new FormControl('', [Validators.required, Validators.pattern(/^[-]?\d+$/)]),
      floorNumber2: new FormControl('', [Validators.required, Validators.pattern(/^[-]?\d+$/)]),
    });
  }

  ngOnInit(): void {
    this.submitSuccessMessage = null;
    this.passageService.getPassages().subscribe((passages) => {
      this.passages = passages;
    });
  }

  getDefaultProps(): UpdatePassageProps {
    return {
      oldPassageSelectedRequiredError: 'Please select an old passage',
      buildingCode1: 'Building 1 Code',
      buildingCode2: 'Building 2 Code',
      floorNumber1: 'Floor 1 Number',
      floorNumber2: 'Floor 2 Number',
      buildingCode1Placeholder: 'Enter Building 1 Code',
      buildingCode1RequiredError: 'Building 1 Code is required',
      buildingCode2Placeholder: 'Enter Building 2 Code',
      buildingCode2RequiredError: 'Building 2 Code is required',
      floorNumber1Placeholder: 'Enter Floor 1 Number',
      floorNumber1RequiredError: 'Floor 1 Number is required',
      floorNumber2Placeholder: 'Enter Floor 2 Number',
      floorNumber2RequiredError: 'Floor 2 Number is required',
      updatePassageButtonLabel: 'Update Passage',
      updatePassageSuccessMessage: 'Passage updated successfully!',
    };
  }

  onSubmit() {
    const passagePosition = this.passageForm.get('oldPassagePosition')?.value;
    const oldPassage = {
      buildingCode1: this.passages[passagePosition].buildingCode1,
      buildingCode2: this.passages[passagePosition].buildingCode2,
      floorNumber1: this.passages[passagePosition].floorNumber1,
      floorNumber2: this.passages[passagePosition].floorNumber2,
    };
    const newPassage = {
      buildingCode1: this.passageForm.get('buildingCode1')?.value,
      buildingCode2: this.passageForm.get('buildingCode2')?.value,
      floorNumber1: this.passageForm.get('floorNumber1')?.value,
      floorNumber2: this.passageForm.get('floorNumber2')?.value,
    };
    this.passageService.updatePassage(oldPassage, newPassage).subscribe({
      next: () => {
        this.submitSuccessMessage = this.props.updatePassageButtonLabel;
        this.isLoading = false;
        this.passageForm.reset();
        this.passageService.getPassages().subscribe((passages) => {
          this.passages = passages;
        });
      },
      error: (error) => {
        console.error('Error creating floor:', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
}
