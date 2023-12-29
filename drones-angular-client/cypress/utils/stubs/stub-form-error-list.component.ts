import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-form-error-list',
  template: '<div>Stubbed Form Error List Component</div>',
})
export class StubFormErrorListComponent {
  @Input() errorResponse: any;
}