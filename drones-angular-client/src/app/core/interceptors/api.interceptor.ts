import { HttpEvent, HttpHandler, HttpInterceptor, HttpRequest } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";
import { API_ROUTES } from "src/api.config";

@Injectable({ providedIn: "root" })
export class ApiInterceptor implements HttpInterceptor {
  intercept(
    req: HttpRequest<any>,
    next: HttpHandler
  ): Observable<HttpEvent<any>> {
    const apiReq = req.clone({ url: `${API_ROUTES.base}${req.url}` });
    return next.handle(apiReq);
  }
}
