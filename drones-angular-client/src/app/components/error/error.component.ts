import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { map } from 'rxjs/operators';

@Component({
  selector: 'app-error',
  templateUrl: './error.component.html',
  styleUrls: ['./error.component.css'],
})
export class ErrorComponent implements OnInit {
  errorCode = '';
  errorMessage = '';
  infoMessage = '';
  private redirectAfter = 0;
  private redirectTo = '';

  constructor(private route: ActivatedRoute, private router: Router) { }

  ngOnInit(): void {
    this.route.data
      .pipe(
        map(
          (data) =>
            data as {
              errorCode: string;
              errorMessage: string;
              infoMessage: string;
              redirectAfter: number;
              redirectTo: string;
            }
        )
      )
      .subscribe(
        (data: {
          errorCode: string;
          errorMessage: string;
          infoMessage: string;
          redirectAfter: number;
          redirectTo: string;
        }) => {
          this.errorCode = data.errorCode;
          this.errorMessage = data.errorMessage;
          this.infoMessage = data.infoMessage;
          this.redirectAfter = data.redirectAfter;
          this.redirectTo = data.redirectTo;

          if (this.redirectAfter && this.redirectTo) {
            setTimeout(() => {
              this.router.navigate([this.redirectTo]);
            }, this.redirectAfter);
          }
        }
      );
  }
}
