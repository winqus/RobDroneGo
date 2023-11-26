import { Component, Input } from '@angular/core';

export type SuccessMessage = string | null;

@Component({
  selector: 'app-success-form-message',
  templateUrl: './success-form-message.component.html',
})
export class SuccessFormMessageComponent {
  @Input() message: SuccessMessage = null;
}
