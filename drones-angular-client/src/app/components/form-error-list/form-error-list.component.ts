import { Component, Input } from '@angular/core';
import { TEXT_TOKENS as content } from '../../../assets/i18n/_textTokens';

@Component({
  selector: 'app-form-error-list',
  templateUrl: './form-error-list.component.html',
})
export class FormErrorListComponent {
  private _errorResponse: any;
  public errorMessages: string[] = [];

  @Input()
  set errorResponse(errorResponse: any) {
    this._errorResponse = errorResponse;
    this.processErrors();
  }

  get errorResponse(): any {
    return this._errorResponse;
  }

  private processErrors(): void {
    const errors: { [key: string]: string } = {};

    if (!this._errorResponse) return;

    // Handling HTTP status 0
    if (this._errorResponse.status === 0) {
      errors['network'] = content.network.error;
    } else if (this._errorResponse.error) {
      if (typeof this._errorResponse.error === 'string') {
        errors['error'] = this._errorResponse.error;
      } else if (typeof this._errorResponse.error?.message === 'string') {
        errors['error'] = this._errorResponse.error.message;
      }
    } else if (this._errorResponse.errors) {
      // 404 Not found error
      Object.assign(errors, this._errorResponse.errors);
    } else if (this._errorResponse.validation) {
      // Celebrate validation error
      this._errorResponse.validation.keys.forEach((key: string) => {
        errors[key] = this._errorResponse.message;
      });
    } else if (this._errorResponse.message) {
      // Simple error or HTTP error
      errors['general'] = this._errorResponse.message;
    }

    this.errorMessages = Object.values(errors);
  }
}
