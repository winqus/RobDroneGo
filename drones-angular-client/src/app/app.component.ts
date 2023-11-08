import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'dronesangularclient';
  trackByTitle(index: number, item: any): any {
    return item.title;
  }
}
