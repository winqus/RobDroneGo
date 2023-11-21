import { HttpClient, HttpRequest, HttpResponse } from '@angular/common/http';
import { Component, Input } from '@angular/core';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import * as marked from 'marked';

@Component({
  selector: 'app-gdpr',
  template: `
    <div class="container mx-auto p-10">
      <div class="bg-base-100 p-4 rounded shadow ">
        <div [innerHTML]="convertedHtml"></div>
      </div>
    </div>
  `,
  styleUrls: ['./gdpr.component.css'],
})
export class GdprComponent {
  @Input() gdprText: any = '';

  convertedHtml: SafeHtml = '';

  constructor(
    private http: HttpClient,
    private sanitizer: DomSanitizer,
  ) {}

  ngOnInit(): void {
    const url = './assets/gdpr/gdpr.md';
    const req = new HttpRequest('GET', url, {
      responseType: 'text',
    });

    this.http.request(req).subscribe({
      next: (event) => {
        if (event instanceof HttpResponse) {
          const markdownContent = event.body;

          this.convertedHtml = this.sanitizer.bypassSecurityTrustHtml(this.markdownToHtml(markdownContent));
        }
      },
      error: (error) => {
        console.error('Error fetching GDPR text:', error);
      },
    });
  }
  markdownToHtml(markdown: any) {
    return marked.parse(markdown);
  }
}
