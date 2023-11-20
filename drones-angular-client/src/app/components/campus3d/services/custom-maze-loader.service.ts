import { HttpClient, HttpEventType, HttpRequest, HttpResponse } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { MapService } from 'src/app/services/map.service';
import MazePartialConfig, { Base3dData, MazeFullConfig } from '../interfaces/mazeData.interface';

@Injectable({
  providedIn: 'root',
})
export class CustomMazeLoaderService {
  constructor(
    private http: HttpClient,
    private mapService: MapService,
  ) {}

  baseMaze3DBody: any;
  mapDataFromApiPreProcessor = (data: any) => data;
  mazeDataPostProcessor = (data: any) => data;

  loadMazeBase3DData(url: string, onLoad: (data: any) => void, onProgress?: (progressEvent: { loaded: number; total: number; percentDone: number }) => void, onError?: (error: any) => void): void {
    const req = new HttpRequest('GET', url, {
      reportProgress: true,
      responseType: 'json',
    });

    this.http.request(req).subscribe({
      next: (event) => {
        if (event.type === HttpEventType.DownloadProgress) {
          if (onProgress && event.total) {
            const percentDone = Math.round((100 * event.loaded) / event.total);
            onProgress({ loaded: event.loaded, total: event.total, percentDone });
          }
        } else if (event instanceof HttpResponse) {
          if (!this.isBaseMaze3DData(event.body)) {
            throw new Error('Base map data does not match Base3dData interface');
          }
          onLoad(event.body);
          this.baseMaze3DBody = event.body;
        }
      },
      error: (err) => {
        if (onError) {
          onError(err);
        }
      },
    });
  }

  isBaseMaze3DData(data: any): data is Base3dData {
    return 'ground' in data && 'wall' in data && 'passageWall' in data && 'elevatorWall' in data && 'doorWall' in data;
  }

  // TODO replace implementation with request to MapService
  load(url: string, onLoad: (data: any) => void, onProgress?: (progressEvent: { loaded: number; total: number; percentDone: number }) => void, onError?: (error: any) => void): void {
    const req = new HttpRequest('GET', url, {
      reportProgress: true,
      responseType: 'json',
    });

    this.http.request(req).subscribe({
      next: (event) => {
        if (event.type === HttpEventType.DownloadProgress) {
          if (onProgress && event.total) {
            const percentDone = Math.round((100 * event.loaded) / event.total);
            onProgress({ loaded: event.loaded, total: event.total, percentDone });
          }
        } else if (event instanceof HttpResponse) {
          onLoad(this.convertToMaze(event.body));
        }
      },
      error: (err) => {
        if (onError) {
          onError(err);
        }
      },
    });
  }

  /**
   * Converts the given data to a MazeData object.
   * All wall/ground/other properties are copied from the baseMaze3DBody object.
   *
   * @param data The data to be converted.
   * @returns The converted MazeData object.
   * @throws Error if the data does not match the MazeOptions interface.
   */
  convertToMaze(data: any): MazeFullConfig {
    data = this.mapDataFromApiPreProcessor(data);

    if (this.isMazeOptions(data)) {
      this.baseMaze3DBody.ground.size = { ...this.baseMaze3DBody.ground.size, ...data.maze.size };
      const fullMazeData: MazeFullConfig = {
        ...data,
        ...this.baseMaze3DBody,
      };
      const processedData = this.mazeDataPostProcessor(fullMazeData);
      return processedData;
    } else {
      throw new Error('Data does not match MazeOptions interface');
    }
  }

  isMazeOptions(data: any): data is MazePartialConfig {
    return 'maze' in data && 'player' in data;
  }
}
