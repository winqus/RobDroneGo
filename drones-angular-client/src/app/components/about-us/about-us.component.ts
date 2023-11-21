import { HttpClient, HttpRequest, HttpResponse } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import * as marked from 'marked';

@Component({
  selector: 'app-about-us',
  template: `
    <div class="container mx-auto p-10">
      <div class="bg-base-100 p-4 rounded shadow ">
        <div [innerHTML]="convertedHtml"></div>
      </div>
    </div>
  `,
  styleUrls: ['./about-us.component.css'],
})
export class AboutUsComponent implements OnInit {
  convertedHtml: SafeHtml = '';

  constructor(
    private http: HttpClient,
    private sanitizer: DomSanitizer,
  ) {}

  ngOnInit(): void {
    const url = './assets/info/info.md'; // Updated path to info.md
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
        console.error('Error fetching info text:', error);
      },
    });
  }

  markdownToHtml(markdown: any) {
    return marked.parse(markdown);
  }
}
