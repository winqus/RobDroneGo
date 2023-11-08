import { Component } from '@angular/core';
import { TEXT_TOKENS as CONTENT } from '../assets/i18n/_textTokens';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'dronesangularclient';
  CONTENT = CONTENT;
  trackByTitle(index: number, item: any): any {
    return item.title;
  }
}
