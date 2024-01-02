import { Component } from '@angular/core';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { filter } from 'rxjs';
import { TEXT_TOKENS as CONTENT } from '../assets/i18n/_textTokens';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
})
export class AppComponent {
  title = 'dronesangularclient';
  CONTENT = CONTENT;

  constructor(
    private router: Router,
    private activatedRoute: ActivatedRoute,
  ) {
    this.router.events.pipe(filter((event) => event instanceof NavigationEnd)).subscribe(() => {
      this.updatePageTitle();
    });
  }

  trackByTitle(index: number, item: any): any {
    return item.title;
  }

  private updatePageTitle() {
    let child: any = this.activatedRoute.firstChild;
    while (child.firstChild) {
      child = child.firstChild;
    }
    if (child.snapshot.data['title']) {
      document.title = `${child.snapshot.data['title']} | ${CONTENT.title}`;
    }
  }
}
