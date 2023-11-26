import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { API_ROUTES } from 'src/api.config';

export interface FileInfo {
  name: string;
  path: string;
}

@Injectable({
  providedIn: 'root',
})
export class FolderService {
  constructor(private http: HttpClient) {}

  listFiles(): Observable<FileInfo[]> {
    return this.http.get<FileInfo[]>(API_ROUTES.folder.list);
  }

  downloadFile(file: FileInfo): Observable<Blob> {
    return this.http.get(API_ROUTES.base + file.path, {
      responseType: 'blob',
    });
  }

  uploadFile(formData: FormData): Observable<any> {
    return this.http.post(API_ROUTES.folder.upload, formData);
  }
}
