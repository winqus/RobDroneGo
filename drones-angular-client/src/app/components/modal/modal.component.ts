import { Component, EventEmitter, Input, Output } from '@angular/core';
import { ModalService } from 'src/app/services/modal.service';

interface ModalButton {
  text: string;
  class: string;
  action: () => void;
}

@Component({
  selector: 'app-modal',
  templateUrl: './modal.component.html',
})
export class ModalComponent {
  @Input() title!: string;
  @Input() body!: string;
  @Input() buttons!: any[];
  @Output() close = new EventEmitter<void>();

  onButtonAction(action: () => void, shouldClose: boolean = false) {
    action();
    if (shouldClose) {
      this.close.emit();
    }
  }
}
