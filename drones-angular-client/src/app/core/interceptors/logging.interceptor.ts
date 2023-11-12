import {
  HTTP_INTERCEPTORS,
  HttpErrorResponse,
  HttpEvent,
  HttpHandler,
  HttpInterceptor,
  HttpRequest,
  HttpResponse
} from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable, throwError } from 'rxjs';
import { catchError, tap } from 'rxjs/operators';
import { environment } from 'src/environments/environment';

@Injectable()
export class LoggingInterceptor implements HttpInterceptor {

  intercept(request: HttpRequest<any>, next: HttpHandler): Observable<HttpEvent<any>> {
    if (!environment.production) { // Only log in non-production environments
      const started = Date.now();
      console.log(`Request started: ${request.method} ${request.urlWithParams}`);

      return next.handle(request).pipe(
        tap(event => {
          if (event instanceof HttpResponse) {
            const elapsed = Date.now() - started;
            console.log(`Request for ${request.urlWithParams} took ${elapsed} ms.`);
          }
        }),
        catchError((error: HttpErrorResponse) => {
          console.error(`Request for ${request.urlWithParams} failed with status ${error.status}: ${error.message}`);
          return throwError(() => error);
        })
      );
    } else {
      return next.handle(request); // In production, simply forward the request without logging
    }
  }
}

export const loggingInterceptorProvider = {
  provide: HTTP_INTERCEPTORS,
  useClass: LoggingInterceptor,
  multi: true
};