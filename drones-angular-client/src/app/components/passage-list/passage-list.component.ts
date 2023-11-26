import { Component, EventEmitter, Input, Output } from '@angular/core';
import { FormControl, FormGroup, ValidationErrors, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import Building from 'src/app/core/models/building.model';
import Passage from 'src/app/core/models/passage.model';
import BuildingService from 'src/app/services/building.service';
import { PassageService } from 'src/app/services/passage.service';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';
import { SuccessMessage } from '../shared/success-form-message/success-form-message.component';

export interface PassageListProps {
  passage1Label: string;
  passage1Placeholder: string;
  passage1RequiredError: string;

  passage2Label: string;
  passage2Placeholder: string;
  passage2RequiredError: string;

  listPassagesButtonLabel: string;
}

@Component({
  selector: 'app-passage-list',
  templateUrl: './passage-list.component.html',
  styleUrls: ['./passage-list.component.css'],
})
export class PassageListComponent {
  buildings: Building[] = [];
  passages: Passage[] = [];
  @Input() props: PassageListProps = this.getDefaultProps();
  @Output() submitEvent = new EventEmitter<unknown>();

  errorResponse: any;
  submitSuccessMessage: SuccessMessage = null;
  noPassagesFoundMessage: string = '';
  isLoading = false;
  loadedOnce = false;
  passageForm: FormGroup;
  validationErrors = content.validation_errors;

  constructor(
    private buildingService: BuildingService,
    private passageService: PassageService,
    private route: ActivatedRoute,
    private router: Router,
  ) {
    this.passageForm = new FormGroup({
      building1: new FormControl('', [Validators.required]),
      building2: new FormControl('', [Validators.required]),
    });
  }
  ngOnInit(): void {
    this.buildingService.getAllBuildings().subscribe((buildings: Building[]) => {
      this.buildings = buildings;
    });
  }

  getDefaultProps(): PassageListProps {
    return {
      passage1Label: 'Building 1',
      passage1Placeholder: 'Enter Building 1',
      passage1RequiredError: 'Building 1 is required',

      passage2Label: 'Building 2',
      passage2Placeholder: 'Enter Building 2',
      passage2RequiredError: 'Building 2 is required',

      listPassagesButtonLabel: 'List Passages',
    };
  }

  getBPassages(building1: string, building2: string) {
    this.passageService.getPassagesBetweenBuildings(building1, building2).subscribe({
      next: (passages: Passage[]) => {
        this.passages = passages;

        if (this.passages.length === 0) {
          this.noPassagesFoundMessage = 'No passages between buildings';
        }
        this.isLoading = false;
        this.loadedOnce = true;
      },
      error: (error: any) => {
        console.error('Error getting passage details', error);
        this.errorResponse = error;
        this.isLoading = false;
      },
    });
  }
  onSubmit() {
    this.isLoading = true;
    this.errorResponse = {};
    this.submitSuccessMessage = null;
    this.noPassagesFoundMessage = '';

    const building1 = this.passageForm.value.building1;
    const building2 = this.passageForm.value.building2;

    this.getBPassages(building1, building2);
  }
}
