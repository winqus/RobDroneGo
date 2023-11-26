import { HttpClient, HttpRequest, HttpResponse } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import * as marked from 'marked';

@Component({
  selector: 'app-complexity-analysis',
  template: `
    <div class="container mx-auto p-10">
      <div class="bg-base-100 p-4 rounded shadow ">
        <div [innerHTML]="convertedHtml"></div>
      </div>
    </div>
  `,
  styleUrls: ['./complexity-analysis.component.css'],
})
export class ComplexityAnalysisComponent implements OnInit {
  convertedHtml: SafeHtml = '';

  constructor(
    private http: HttpClient,
    private sanitizer: DomSanitizer,
  ) {}

  ngOnInit(): void {
    const url = './assets/complexityanalysis/analysis.md';
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
        console.error('Error fetching analysis text:', error);
      },
    });
  }

  markdownToHtml(markdown: any) {
    return marked.parse(markdown);
  }
}
